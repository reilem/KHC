data Number = Zero

-- This now works
case Zero of
  Zero -> Zero
  Zero -> Zero

-- This doesn't
-- (\x. case x of
--   Zero -> Zero
--   Zero -> Zero) Zero
