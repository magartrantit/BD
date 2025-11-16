
CREATE EXTENSION IF NOT EXISTS citext;
CREATE EXTENSION IF NOT EXISTS btree_gist;

-- === 1) LOCATION & ROOM ===
CREATE TABLE location (
  id        BIGSERIAL PRIMARY KEY,
  name      TEXT    NOT NULL,
  address   TEXT,
  city      TEXT,
  country   TEXT DEFAULT 'RO'
);

CREATE TABLE room (
  id          BIGSERIAL PRIMARY KEY,
  location_id BIGINT NOT NULL REFERENCES location(id) ON DELETE RESTRICT,
  name        TEXT   NOT NULL,
  room_type   TEXT   NOT NULL,
  capacity    INTEGER NOT NULL CHECK (capacity > 0),
  status      TEXT NOT NULL DEFAULT 'active',
  UNIQUE (location_id, name)
);

-- === 2) USERS & MEMBERS ===
CREATE TABLE user_account (
  id        BIGSERIAL PRIMARY KEY,
  email     CITEXT NOT NULL UNIQUE,
  role      TEXT   NOT NULL DEFAULT 'member',
  is_active BOOLEAN NOT NULL DEFAULT TRUE
);

CREATE TABLE member_profile (
  id              BIGSERIAL PRIMARY KEY,
  user_id         BIGINT NOT NULL UNIQUE REFERENCES user_account(id) ON DELETE CASCADE,
  full_name       TEXT   NOT NULL,
  phone           TEXT,
  billing_address TEXT
);

-- === 3) TAX CATEGORY & TAX RATE ===
CREATE TABLE tax_category (
  id   BIGSERIAL PRIMARY KEY,
  code TEXT NOT NULL UNIQUE,
  name TEXT NOT NULL
);

CREATE TABLE tax_rate (
  id              BIGSERIAL PRIMARY KEY,
  tax_category_id BIGINT NOT NULL REFERENCES tax_category(id) ON DELETE RESTRICT,
  validity        TSTZRANGE NOT NULL,
  rate_numeric    NUMERIC(5,2) NOT NULL CHECK (rate_numeric >= 0),
  EXCLUDE USING gist (tax_category_id WITH =, validity WITH &&) 
);

-- === 4) PRODUCT ===
CREATE TABLE product (
  id              BIGSERIAL PRIMARY KEY,
  name            TEXT NOT NULL,
  tax_category_id BIGINT NOT NULL REFERENCES tax_category(id) ON DELETE RESTRICT,
  is_active       BOOLEAN NOT NULL DEFAULT TRUE
);

-- === 5) PRICELIST & ROOM_RATE ===
CREATE TABLE pricelist (
  id       BIGSERIAL PRIMARY KEY,
  name     TEXT NOT NULL,
  validity TSTZRANGE NOT NULL
);

CREATE TABLE room_rate (
  id              BIGSERIAL PRIMARY KEY,
  pricelist_id    BIGINT NOT NULL REFERENCES pricelist(id) ON DELETE CASCADE,
  room_id         BIGINT REFERENCES room(id) ON DELETE CASCADE,
  room_type       TEXT,
  validity        TSTZRANGE NOT NULL,
  unit            TEXT NOT NULL CHECK (unit IN ('hour','day')),
  unit_price      NUMERIC(12,2) NOT NULL CHECK (unit_price >= 0),
  tax_category_id BIGINT NOT NULL REFERENCES tax_category(id) ON DELETE RESTRICT,
  CHECK ( (room_id IS NOT NULL) <> (room_type IS NOT NULL) ),
  EXCLUDE USING gist (
    COALESCE(room_id,0) WITH =,
    COALESCE(room_type,'') WITH =,
    pricelist_id WITH =,
    validity WITH &&
  )
);

-- === 6) BOOKING & BOOKING_ITEM ===
CREATE TABLE booking (
  id          BIGSERIAL PRIMARY KEY,
  room_id     BIGINT NOT NULL REFERENCES room(id) ON DELETE RESTRICT,
  member_id   BIGINT REFERENCES member_profile(id) ON DELETE SET NULL,
  starts_at   TIMESTAMPTZ NOT NULL,
  ends_at     TIMESTAMPTZ NOT NULL,
  status      TEXT NOT NULL DEFAULT 'draft',
  seats       INTEGER NOT NULL DEFAULT 1 CHECK (seats > 0),
  
  period      TSTZRANGE GENERATED ALWAYS AS (tstzrange(starts_at, ends_at, '[)')) STORED,
  duration_min INTEGER  GENERATED ALWAYS AS (GREATEST(0, (EXTRACT(EPOCH FROM (ends_at - starts_at)) / 60)::INT)) STORED,
  total_amount_denorm     NUMERIC(12,2) NOT NULL DEFAULT 0,
  currency                TEXT NOT NULL DEFAULT 'RON',
  cancellation_fee_denorm NUMERIC(12,2) NOT NULL DEFAULT 0,
  CHECK (ends_at > starts_at)
);

CREATE INDEX IF NOT EXISTS booking_period_gist ON booking USING gist (room_id, period);
ALTER TABLE booking
  ADD CONSTRAINT booking_no_overlap
  EXCLUDE USING gist (room_id WITH =, period WITH &&);

CREATE TABLE booking_item (
  id              BIGSERIAL PRIMARY KEY,
  booking_id      BIGINT NOT NULL REFERENCES booking(id) ON DELETE CASCADE,
  item_type       TEXT   NOT NULL,               
  product_id      BIGINT REFERENCES product(id),
  quantity        NUMERIC(12,2) NOT NULL DEFAULT 1 CHECK (quantity > 0),
  unit_price      NUMERIC(12,2) NOT NULL CHECK (unit_price >= 0),
  tax_category_id BIGINT NOT NULL REFERENCES tax_category(id) ON DELETE RESTRICT,
  notes           TEXT
);

-- === 7) INVOICE, INVOICE_LINE, PAYMENT, CREDIT_NOTE ===
CREATE TABLE invoice (
  id                 BIGSERIAL PRIMARY KEY,
  customer_id        BIGINT REFERENCES member_profile(id) ON DELETE SET NULL,
  issue_date         DATE NOT NULL,
  due_date           DATE NOT NULL,
  status             TEXT NOT NULL DEFAULT 'draft',
  currency           TEXT NOT NULL DEFAULT 'RON',
  total_net_denorm   NUMERIC(12,2) NOT NULL DEFAULT 0,
  total_vat_denorm   NUMERIC(12,2) NOT NULL DEFAULT 0,
  total_gross_denorm NUMERIC(12,2) NOT NULL DEFAULT 0
);

CREATE TABLE invoice_line (
  id               BIGSERIAL PRIMARY KEY,
  invoice_id       BIGINT NOT NULL REFERENCES invoice(id) ON DELETE CASCADE,
  source_type      TEXT,
  source_id        BIGINT,
  description      TEXT NOT NULL,
  quantity         NUMERIC(12,2) NOT NULL DEFAULT 1 CHECK (quantity > 0),
  unit_price       NUMERIC(12,2) NOT NULL CHECK (unit_price >= 0),
  tax_category_id  BIGINT NOT NULL REFERENCES tax_category(id) ON DELETE RESTRICT,
  applied_vat_rate NUMERIC(5,2),          -- <== FĂRĂ NOT NULL, FĂRĂ DEFAULT
  vat_amount       NUMERIC(12,2) NOT NULL DEFAULT 0,
  line_total       NUMERIC(12,2) NOT NULL DEFAULT 0
);


CREATE TABLE payment (
  id         BIGSERIAL PRIMARY KEY,
  invoice_id BIGINT NOT NULL REFERENCES invoice(id) ON DELETE CASCADE,
  method     TEXT   NOT NULL,         
  amount     NUMERIC(12,2) NOT NULL CHECK (amount > 0),
  paid_at    TIMESTAMPTZ NOT NULL,
  status     TEXT NOT NULL DEFAULT 'settled',  
  reference  TEXT
);

CREATE TABLE credit_note (
  id           BIGSERIAL PRIMARY KEY,
  invoice_id   BIGINT NOT NULL REFERENCES invoice(id) ON DELETE CASCADE,
  reason       TEXT,
  issue_date   DATE NOT NULL,
  total_amount NUMERIC(12,2) NOT NULL CHECK (total_amount >= 0)
);

-- === 8) WALLET & WALLET_TX ===
CREATE TABLE wallet (
  member_id      BIGINT PRIMARY KEY REFERENCES member_profile(id) ON DELETE CASCADE,
  balance_denorm NUMERIC(12,2) NOT NULL DEFAULT 0
);

CREATE TABLE wallet_tx (
  id          BIGSERIAL PRIMARY KEY,
  member_id   BIGINT NOT NULL REFERENCES member_profile(id) ON DELETE CASCADE,
  occurred_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  amount      NUMERIC(12,2) NOT NULL,  
  reason      TEXT NOT NULL,
  ref_type    TEXT,
  ref_id      BIGINT
);

-- === 9) ATTENDANCE & MAINTENANCE_WINDOW ===
CREATE TABLE attendance (
  id          BIGSERIAL PRIMARY KEY,
  booking_id  BIGINT NOT NULL REFERENCES booking(id) ON DELETE CASCADE,
  checkin_at  TIMESTAMPTZ,
  checkout_at TIMESTAMPTZ,
  method      TEXT,                   
  CHECK (checkout_at IS NULL OR checkout_at >= checkin_at)
);

CREATE TABLE maintenance_window (
  id      BIGSERIAL PRIMARY KEY,
  room_id BIGINT NOT NULL REFERENCES room(id) ON DELETE CASCADE,
  reason  TEXT,
  period  TSTZRANGE NOT NULL
);

-- === 10) AUDIT_LOG ===
CREATE TABLE audit_log (
  id          BIGSERIAL PRIMARY KEY,
  actor_id    BIGINT REFERENCES user_account(id) ON DELETE SET NULL,
  occurred_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  action      TEXT NOT NULL,
  entity_type TEXT NOT NULL,
  entity_id   BIGINT,
  details     JSONB
);

-- === 11) SUMAR: DAILY_ROOM_UTILIZATION & MONTHLY_LOCATION_REVENUE ===
CREATE TABLE daily_room_utilization (
  location_id         BIGINT NOT NULL REFERENCES location(id) ON DELETE CASCADE,
  room_id             BIGINT NOT NULL REFERENCES room(id) ON DELETE CASCADE,
  day                 DATE   NOT NULL,
  minutes_booked      INTEGER NOT NULL DEFAULT 0,
  minutes_maintenance INTEGER NOT NULL DEFAULT 0,
  minutes_free        INTEGER NOT NULL DEFAULT 0,
  PRIMARY KEY (room_id, day)
);

CREATE TABLE monthly_location_revenue (
  location_id   BIGINT NOT NULL REFERENCES location(id) ON DELETE CASCADE,
  year_month    CHAR(7) NOT NULL, 
  revenue_net   NUMERIC(14,2) NOT NULL DEFAULT 0,
  revenue_vat   NUMERIC(14,2) NOT NULL DEFAULT 0,
  revenue_gross NUMERIC(14,2) NOT NULL DEFAULT 0,
  PRIMARY KEY (location_id, year_month)
);

-- === 12) Indici utili ===
CREATE INDEX IF NOT EXISTS idx_room_location ON room(location_id);
CREATE INDEX IF NOT EXISTS idx_booking_room_starts ON booking(room_id, starts_at);
CREATE INDEX IF NOT EXISTS idx_invoice_issue_date ON invoice(issue_date);
CREATE INDEX IF NOT EXISTS idx_payment_invoice ON payment(invoice_id);
CREATE INDEX IF NOT EXISTS idx_wallettx_member ON wallet_tx(member_id, occurred_at);
