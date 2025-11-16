CREATE EXTENSION IF NOT EXISTS btree_gist;

-- 0) Utilitar rotunjire monetară
CREATE OR REPLACE FUNCTION f_round_money(p_value numeric)
RETURNS numeric LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
  SELECT ROUND(p_value, 2)
$$;

-- 1) Rata TVA valabilă la un moment dat
CREATE OR REPLACE FUNCTION get_tax_rate_at(p_tax_category_id bigint, p_ts timestamptz)
RETURNS numeric LANGUAGE sql STABLE AS $$
  SELECT COALESCE((
    SELECT rate_numeric
    FROM tax_rate
    WHERE tax_category_id = p_tax_category_id
      AND validity @> p_ts
    ORDER BY LOWER(validity) DESC
    LIMIT 1
  ), 0)
$$;

-- 2) Validare: booking să nu se suprapună cu mentenanța
CREATE OR REPLACE FUNCTION trg_booking_validate_maintenance()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  v_cnt int;
BEGIN
  SELECT COUNT(*) INTO v_cnt
  FROM maintenance_window m
  WHERE m.room_id = NEW.room_id
    AND m.period && NEW.period;

  IF v_cnt > 0 THEN
    RAISE EXCEPTION USING
      MESSAGE = 'Booking overlaps a maintenance window',
      DETAIL  = 'room_id='||NEW.room_id||' period='||NEW.period::text,
      HINT    = 'Reschedule booking or close maintenance first.';
  END IF;

  IF NEW.ends_at <= NEW.starts_at THEN
    RAISE EXCEPTION 'Invalid booking interval: ends_at (%) must be after starts_at (%)',
      NEW.ends_at, NEW.starts_at;
  END IF;

  RETURN NEW;
END$$;

DROP TRIGGER IF EXISTS booking_validate_maintenance ON booking;
CREATE TRIGGER booking_validate_maintenance
BEFORE INSERT OR UPDATE OF room_id, starts_at, ends_at ON booking
FOR EACH ROW EXECUTE FUNCTION trg_booking_validate_maintenance();

-- 3) Booking: total denormalizat (gross)
CREATE OR REPLACE FUNCTION f_booking_compute_total(p_booking_id bigint)
RETURNS numeric LANGUAGE plpgsql STABLE AS $$
DECLARE
  v_starts timestamptz;
  v_total numeric := 0;
BEGIN
  SELECT b.starts_at INTO v_starts FROM booking b WHERE b.id = p_booking_id;
  IF v_starts IS NULL THEN
    RETURN 0;
  END IF;

  SELECT COALESCE(SUM(
           f_round_money(bi.quantity * bi.unit_price) +
           f_round_money((bi.quantity * bi.unit_price) *
                         get_tax_rate_at(bi.tax_category_id, v_starts) / 100)
         ), 0)
  INTO v_total
  FROM booking_item bi
  WHERE bi.booking_id = p_booking_id;

  RETURN v_total;
END$$;

CREATE OR REPLACE FUNCTION trg_booking_items_retotal()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  UPDATE booking b
     SET total_amount_denorm = f_booking_compute_total(b.id)
   WHERE b.id = COALESCE(NEW.booking_id, OLD.booking_id);
  RETURN COALESCE(NEW, OLD);
END$$;

DROP TRIGGER IF EXISTS booking_item_aiud_retotal ON booking_item;
CREATE TRIGGER booking_item_aiud_retotal
AFTER INSERT OR UPDATE OR DELETE ON booking_item
FOR EACH ROW EXECUTE FUNCTION trg_booking_items_retotal();

-- 4) Invoice_line: calcul TVA și total linie
CREATE OR REPLACE FUNCTION trg_invoice_line_fill_and_amounts()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  v_issue_ts timestamptz;
  v_net numeric;
  v_vat_rate numeric;
  v_vat numeric;
BEGIN
  SELECT i.issue_date::timestamptz INTO v_issue_ts
  FROM invoice i
  WHERE i.id = NEW.invoice_id;

  IF v_issue_ts IS NULL THEN
    RAISE EXCEPTION 'Invoice % not found for invoice_line', NEW.invoice_id;
  END IF;

  IF NEW.applied_vat_rate IS NULL THEN
    NEW.applied_vat_rate := get_tax_rate_at(NEW.tax_category_id, v_issue_ts);
  END IF;

  v_vat_rate := NEW.applied_vat_rate;
  v_net := f_round_money(NEW.quantity * NEW.unit_price);
  v_vat := f_round_money(v_net * v_vat_rate / 100);

  NEW.vat_amount := v_vat;
  NEW.line_total := v_net + v_vat;

  RETURN NEW;
END$$;

DROP TRIGGER IF EXISTS invoice_line_biub_fill ON invoice_line;
CREATE TRIGGER invoice_line_biub_fill
BEFORE INSERT OR UPDATE OF quantity, unit_price, tax_category_id, applied_vat_rate ON invoice_line
FOR EACH ROW EXECUTE FUNCTION trg_invoice_line_fill_and_amounts();

-- 5) Invoice: totaluri denormalizate + status după plăți
CREATE OR REPLACE FUNCTION f_invoice_retotal(p_invoice_id bigint)
RETURNS void LANGUAGE plpgsql AS $$
DECLARE
  v_net numeric;
  v_vat numeric;
  v_total numeric;
BEGIN
  SELECT COALESCE(SUM(il.quantity * il.unit_price),0),
         COALESCE(SUM(il.vat_amount),0),
         COALESCE(SUM(il.line_total),0)
    INTO v_net, v_vat, v_total
  FROM invoice_line il
  WHERE il.invoice_id = p_invoice_id;

  UPDATE invoice i
     SET total_net_denorm   = f_round_money(v_net),
         total_vat_denorm   = f_round_money(v_vat),
         total_gross_denorm = f_round_money(v_total)
   WHERE i.id = p_invoice_id;
END$$;

CREATE OR REPLACE FUNCTION trg_invoice_lines_retotal()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  PERFORM f_invoice_retotal(COALESCE(NEW.invoice_id, OLD.invoice_id));
  RETURN COALESCE(NEW, OLD);
END$$;

DROP TRIGGER IF EXISTS invoice_line_aiud_retotal ON invoice_line;
CREATE TRIGGER invoice_line_aiud_retotal
AFTER INSERT OR UPDATE OR DELETE ON invoice_line
FOR EACH ROW EXECUTE FUNCTION trg_invoice_lines_retotal();

CREATE OR REPLACE FUNCTION trg_payment_update_invoice_status()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  v_invoice_id bigint;
  v_paid numeric;
  v_total numeric;
BEGIN
  v_invoice_id := COALESCE(NEW.invoice_id, OLD.invoice_id);

  SELECT COALESCE(SUM(p.amount),0) INTO v_paid
    FROM payment p
   WHERE p.invoice_id = v_invoice_id
     AND p.status = 'settled';

  SELECT total_gross_denorm INTO v_total
  FROM invoice
  WHERE id = v_invoice_id;

  IF v_total IS NULL THEN
    RETURN COALESCE(NEW, OLD);
  END IF;

  UPDATE invoice
     SET status = CASE
                    WHEN v_paid >= v_total THEN 'paid'
                    WHEN v_paid > 0 THEN 'issued'
                    ELSE status
                  END
   WHERE id = v_invoice_id;

  RETURN COALESCE(NEW, OLD);
END$$;

DROP TRIGGER IF EXISTS payment_aiud_invoice_status ON payment;
CREATE TRIGGER payment_aiud_invoice_status
AFTER INSERT OR UPDATE OR DELETE ON payment
FOR EACH ROW EXECUTE FUNCTION trg_payment_update_invoice_status();

-- 6) Wallet: menținere balance_denorm
CREATE OR REPLACE FUNCTION trg_wallet_tx_balance()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  v_member_id bigint;
  v_delta numeric := 0;
BEGIN
  IF TG_OP = 'INSERT' THEN
    v_member_id := NEW.member_id;
    v_delta := NEW.amount;
  ELSIF TG_OP = 'DELETE' THEN
    v_member_id := OLD.member_id;
    v_delta := -OLD.amount;
  ELSE
    v_member_id := NEW.member_id;
    v_delta := NEW.amount - OLD.amount;
  END IF;

  INSERT INTO wallet(member_id, balance_denorm)
  VALUES (v_member_id, f_round_money(v_delta))
  ON CONFLICT (member_id)
  DO UPDATE SET balance_denorm =
    f_round_money(wallet.balance_denorm + EXCLUDED.balance_denorm);

  RETURN COALESCE(NEW, OLD);
END$$;

DROP TRIGGER IF EXISTS wallet_tx_aiud_balance ON wallet_tx;
CREATE TRIGGER wallet_tx_aiud_balance
AFTER INSERT OR UPDATE OR DELETE ON wallet_tx
FOR EACH ROW EXECUTE FUNCTION trg_wallet_tx_balance();

-- 7) Utilizare zilnică săli
CREATE OR REPLACE FUNCTION f_minutes_overlap(
  p_start timestamptz,
  p_end   timestamptz,
  p_day   date
) RETURNS int LANGUAGE plpgsql IMMUTABLE AS $$
DECLARE
  v_a timestamptz := GREATEST(p_start, p_day::timestamptz);
  v_b timestamptz := LEAST(p_end, (p_day + 1)::timestamptz);
  v_min int;
BEGIN
  IF v_b <= v_a THEN
    RETURN 0;
  END IF;
  v_min := CEIL(EXTRACT(EPOCH FROM (v_b - v_a)) / 60.0);
  RETURN v_min;
END$$;

CREATE OR REPLACE FUNCTION f_refresh_daily_room_utilization(
  p_room_id bigint,
  p_day     date
) RETURNS void LANGUAGE plpgsql AS $$
DECLARE
  v_location_id bigint;
  v_booked int;
  v_maint int;
BEGIN
  SELECT r.location_id INTO v_location_id
  FROM room r
  WHERE r.id = p_room_id;

  SELECT COALESCE(SUM(f_minutes_overlap(LOWER(b.period),
                                        UPPER(b.period),
                                        p_day)), 0)
    INTO v_booked
  FROM booking b
  WHERE b.room_id = p_room_id
    AND b.status IN ('confirmed','completed')
    AND b.period && tstzrange(p_day::timestamptz,
                              (p_day + 1)::timestamptz,
                              '[)');

  SELECT COALESCE(SUM(f_minutes_overlap(LOWER(m.period),
                                        UPPER(m.period),
                                        p_day)), 0)
    INTO v_maint
  FROM maintenance_window m
  WHERE m.room_id = p_room_id
    AND m.period && tstzrange(p_day::timestamptz,
                              (p_day + 1)::timestamptz,
                              '[)');

  INSERT INTO daily_room_utilization(
    location_id, room_id, day,
    minutes_booked, minutes_maintenance, minutes_free
  )
  VALUES (
    v_location_id, p_room_id, p_day,
    v_booked, v_maint, GREATEST(0, 1440 - v_booked - v_maint)
  )
  ON CONFLICT (room_id, day)
  DO UPDATE SET
    location_id         = EXCLUDED.location_id,
    minutes_booked      = EXCLUDED.minutes_booked,
    minutes_maintenance = EXCLUDED.minutes_maintenance,
    minutes_free        = EXCLUDED.minutes_free;
END$$;

CREATE OR REPLACE FUNCTION trg_refresh_utilization_from_booking()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  v_day date;
BEGIN
  IF TG_OP IN ('INSERT','UPDATE') THEN
    v_day := (NEW.starts_at AT TIME ZONE 'UTC')::date;
    WHILE v_day <= (NEW.ends_at - INTERVAL '1 second')::date LOOP
      PERFORM f_refresh_daily_room_utilization(NEW.room_id, v_day);
      v_day := v_day + 1;
    END LOOP;
  END IF;

  IF TG_OP IN ('UPDATE','DELETE') THEN
    v_day := (OLD.starts_at AT TIME ZONE 'UTC')::date;
    WHILE v_day <= (OLD.ends_at - INTERVAL '1 second')::date LOOP
      PERFORM f_refresh_daily_room_utilization(OLD.room_id, v_day);
      v_day := v_day + 1;
    END LOOP;
  END IF;

  RETURN COALESCE(NEW, OLD);
END$$;

DROP TRIGGER IF EXISTS booking_aiud_refresh_utilization ON booking;
CREATE TRIGGER booking_aiud_refresh_utilization
AFTER INSERT OR UPDATE OF room_id, starts_at, ends_at, status ON booking
FOR EACH ROW EXECUTE FUNCTION trg_refresh_utilization_from_booking();

CREATE OR REPLACE FUNCTION trg_refresh_utilization_from_maintenance()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  v_day date;
  v_start timestamptz;
  v_end   timestamptz;
  v_room  bigint;
BEGIN
  IF TG_OP IN ('INSERT','UPDATE') THEN
    v_room := NEW.room_id;
    v_start := LOWER(NEW.period);
    v_end := UPPER(NEW.period);
  ELSE
    v_room := OLD.room_id;
    v_start := LOWER(OLD.period);
    v_end := UPPER(OLD.period);
  END IF;

  v_day := (v_start AT TIME ZONE 'UTC')::date;
  WHILE v_day <= (v_end - INTERVAL '1 second')::date LOOP
    PERFORM f_refresh_daily_room_utilization(v_room, v_day);
    v_day := v_day + 1;
  END LOOP;

  RETURN COALESCE(NEW, OLD);
END$$;

DROP TRIGGER IF EXISTS maintenance_aiud_refresh_utilization ON maintenance_window;
CREATE TRIGGER maintenance_aiud_refresh_utilization
AFTER INSERT OR UPDATE OR DELETE ON maintenance_window
FOR EACH ROW EXECUTE FUNCTION trg_refresh_utilization_from_maintenance();

-- 8) Procedură: facturare booking
CREATE OR REPLACE FUNCTION sp_invoice_booking(p_booking_id bigint)
RETURNS bigint LANGUAGE plpgsql AS $$
DECLARE
  v_inv_id bigint;
  v_member bigint;
  v_issue date := CURRENT_DATE;
  v_due   date := CURRENT_DATE + 7;
BEGIN
  SELECT b.member_id INTO v_member
  FROM booking b
  WHERE b.id = p_booking_id;

  IF v_member IS NULL THEN
    RAISE EXCEPTION 'Booking % has no member to bill', p_booking_id;
  END IF;

  INSERT INTO invoice(customer_id, issue_date, due_date, status, currency)
  SELECT v_member, v_issue, v_due, 'issued', b.currency
  FROM booking b
  WHERE b.id = p_booking_id
  RETURNING id INTO v_inv_id;

  INSERT INTO invoice_line(
    invoice_id, source_type, source_id, description,
    quantity, unit_price, tax_category_id
  )
  SELECT v_inv_id, 'booking', bi.booking_id,
         COALESCE('Booking #'||bi.booking_id||' - '||bi.item_type, 'Booking item'),
         bi.quantity, bi.unit_price, bi.tax_category_id
  FROM booking_item bi
  WHERE bi.booking_id = p_booking_id;

  PERFORM f_invoice_retotal(v_inv_id);

  RETURN v_inv_id;
END$$;

-- 9) Revenue lunar pe locație (din linii de tip booking)
CREATE OR REPLACE FUNCTION f_refresh_monthly_location_revenue(
  p_loc_id    bigint,
  p_year_month char(7)
) RETURNS void LANGUAGE plpgsql AS $$
DECLARE
  v_start date := to_date(p_year_month||'-01', 'YYYY-MM-DD');
  v_end   date := (v_start + INTERVAL '1 month');
  v_net numeric;
  v_vat numeric;
  v_gross numeric;
BEGIN
  SELECT COALESCE(SUM(il.quantity * il.unit_price),0),
         COALESCE(SUM(il.vat_amount),0),
         COALESCE(SUM(il.line_total),0)
    INTO v_net, v_vat, v_gross
  FROM invoice i
  JOIN invoice_line il ON il.invoice_id = i.id AND il.source_type = 'booking'
  JOIN booking b ON b.id = il.source_id
  JOIN room r ON r.id = b.room_id
  WHERE r.location_id = p_loc_id
    AND i.issue_date >= v_start
    AND i.issue_date <  v_end;

  INSERT INTO monthly_location_revenue(
    location_id, year_month,
    revenue_net, revenue_vat, revenue_gross
  )
  VALUES (
    p_loc_id, p_year_month,
    f_round_money(v_net),
    f_round_money(v_vat),
    f_round_money(v_gross)
  )
  ON CONFLICT (location_id, year_month)
  DO UPDATE SET
    revenue_net   = EXCLUDED.revenue_net,
    revenue_vat   = EXCLUDED.revenue_vat,
    revenue_gross = EXCLUDED.revenue_gross;
END$$;

CREATE OR REPLACE FUNCTION trg_monthly_revenue_from_invoice_line()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  v_loc  bigint;
  v_ym   char(7);
  v_date date;
BEGIN
  IF TG_OP = 'DELETE' THEN
    IF OLD.source_type <> 'booking' THEN
      RETURN OLD;
    END IF;

    SELECT r.location_id, i.issue_date
      INTO v_loc, v_date
    FROM invoice i
    JOIN booking b ON b.id = OLD.source_id
    JOIN room r ON r.id = b.room_id
    WHERE i.id = OLD.invoice_id;
  ELSE
    IF NEW.source_type <> 'booking' THEN
      RETURN NEW;
    END IF;

    SELECT r.location_id, i.issue_date
      INTO v_loc, v_date
    FROM invoice i
    JOIN booking b ON b.id = NEW.source_id
    JOIN room r ON r.id = b.room_id
    WHERE i.id = NEW.invoice_id;
  END IF;

  IF v_loc IS NULL OR v_date IS NULL THEN
    RETURN COALESCE(NEW, OLD);
  END IF;

  v_ym := to_char(v_date, 'YYYY-MM');
  PERFORM f_refresh_monthly_location_revenue(v_loc, v_ym);

  RETURN COALESCE(NEW, OLD);
END$$;

DROP TRIGGER IF EXISTS invoice_line_aiud_monthly_rev ON invoice_line;
CREATE TRIGGER invoice_line_aiud_monthly_rev
AFTER INSERT OR UPDATE OR DELETE ON invoice_line
FOR EACH ROW EXECUTE FUNCTION trg_monthly_revenue_from_invoice_line();

-- 10) Procedură utilitară: creare booking + items atomic
CREATE OR REPLACE FUNCTION sp_create_booking_with_items(
  p_room_id   bigint,
  p_member_id bigint,
  p_starts_at timestamptz,
  p_ends_at   timestamptz,
  p_currency  text,
  p_items     jsonb
) RETURNS bigint LANGUAGE plpgsql AS $$
DECLARE
  v_bid bigint;
  v_it jsonb;
BEGIN
  INSERT INTO booking(
    room_id, member_id, starts_at, ends_at,
    status, seats, currency
  )
  VALUES (
    p_room_id, p_member_id,
    p_starts_at, p_ends_at,
    'confirmed', 1, p_currency
  )
  RETURNING id INTO v_bid;

  FOR v_it IN
    SELECT * FROM jsonb_array_elements(p_items)
  LOOP
    INSERT INTO booking_item(
      booking_id, item_type, product_id,
      quantity, unit_price, tax_category_id, notes
    )
    VALUES (
      v_bid,
      COALESCE(v_it->>'item_type','room_time'),
      (v_it->>'product_id')::bigint,
      COALESCE((v_it->>'quantity')::numeric,1),
      COALESCE((v_it->>'unit_price')::numeric,0),
      (v_it->>'tax_category_id')::bigint,
      v_it->>'notes'
    );
  END LOOP;

  RETURN v_bid;
END$$;
