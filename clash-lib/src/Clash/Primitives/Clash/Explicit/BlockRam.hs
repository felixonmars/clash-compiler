module Clash.Primitives.Clash.GHC.Word where

import qualified Clash.GHC.Word as P

systemverilogPrimitives =
  [ -- blockRam#
    --   :: ( KnownDomain dom        ARG[0]
    --      , HasCallStack  --       ARG[1]
    --      , Undefined a ) --       ARG[2]
    --   => Clock dom       -- clk,  ARG[3]
    --   -> Enable dom      -- en,   ARG[4]
    --   -> Vec n a         -- init, ARG[5]
    --   -> Signal dom Int  -- rd,   ARG[6]
    --   -> Signal dom Bool -- wren, ARG[7]
    --   -> Signal dom Int  -- wr,   ARG[8]
    --   -> Signal dom a    -- din,  ARG[9]
    --   -> Signal dom a
    (blackbox P.blockRam#){
      kind=Declaration
    , template=[I.i|
        // blockRam begin
        ~SIGD[~GENSYM[RAM][1]][5];
        logic [~SIZE[~TYP[9]]-1:0] ~GENSYM[~RESULT_q][2];
        initial begin
          ~SYM[1] = ~CONST[5];
        end~IF ~ISACTIVEENABLE[4] ~THEN
        always @(~IF~ACTIVEEDGE[Rising][0]~THENposedge~ELSEnegedge~FI ~ARG[3]) begin : ~GENSYM[~COMPNAME_blockRam][3]~IF ~VIVADO ~THEN
          if (~ARG[4]) begin
            if (~ARG[7]) begin
              ~SYM[1][~ARG[8]] <= ~TOBV[~ARG[9]][~TYP[9]];
            end
            ~SYM[2] <= ~SYM[1][~ARG[6]];
          end~ELSE
          if (~ARG[7] & ~ARG[4]) begin
            ~SYM[1][~ARG[8]] <= ~TOBV[~ARG[9]][~TYP[9]];
          end
          if (~ARG[4]) begin
            ~SYM[2] <= ~SYM[1][~ARG[6]];
          end~FI
        end~ELSE
        always @(~IF~ACTIVEEDGE[Rising][0]~THENposedge~ELSEnegedge~FI ~ARG[3]) begin : ~SYM[3]
          if (~ARG[7]) begin
            ~SYM[1][~ARG[8]] <= ~TOBV[~ARG[9]][~TYP[9]];
          end
          ~SYM[2] <= ~SYM[1][~ARG[6]];
        end~FI
        assign ~RESULT = ~FROMBV[~SYM[2]][~TYP[9]];
        // blockRam end
      |]
    }
    -- blockRamU#
    --   :: ( KnownDomain dom        ARG[0]
    --      , HasCallStack  --       ARG[1]
    --      , Undefined a ) --       ARG[2]
    --   => Clock dom       -- clk,  ARG[3]
    --   -> Enable dom      -- en,   ARG[4]
    --   -> SNat n          -- len,  ARG[5]
    --   -> Signal dom Int  -- rd,   ARG[6]
    --   -> Signal dom Bool -- wren, ARG[7]
    --   -> Signal dom Int  -- wr,   ARG[8]
    --   -> Signal dom a    -- din,  ARG[9]
    --   -> Signal dom a
  , (blackbox P.blockRamU#){
      kind=Declaration
    , template=[I.i|
        // blockRamU begin,
        ~TYPO ~GENSYM[~RESULT_RAM][1] [0:~LIT[5]-1];
        logic [~SIZE[~TYP[9]]-1:0] ~GENSYM[~RESULT_q][2];~IF ~ISACTIVEENABLE[4] ~THEN
        always @(~IF~ACTIVEEDGE[Rising][0]~THENposedge~ELSEnegedge~FI ~ARG[3]) begin : ~GENSYM[~COMPNAME_blockRam][3]~IF ~VIVADO ~THEN
          if (~ARG[4]) begin
            if (~ARG[7]) begin
              ~SYM[1][~ARG[8]] <= ~TOBV[~ARG[9]][~TYP[9]];
            end
            ~SYM[2] <= ~SYM[1][~ARG[6]];
          end~ELSE
          if (~ARG[7] & ~ARG[4]) begin
            ~SYM[1][~ARG[8]] <= ~TOBV[~ARG[9]][~TYP[9]];
          end
          if (~ARG[4]) begin
            ~SYM[2] <= ~SYM[1][~ARG[6]];
          end~FI
        end~ELSE
        always @(~IF~ACTIVEEDGE[Rising][0]~THENposedge~ELSEnegedge~FI ~ARG[3]) begin : ~SYM[3]
          if (~ARG[7]) begin
            ~SYM[1][~ARG[8]] <= ~TOBV[~ARG[9]][~TYP[9]];
          end
          ~SYM[2] <= ~SYM[1][~ARG[6]];
        end~FI
        assign ~RESULT = ~FROMBV[~SYM[2]][~TYP[9]];
        // blockRamU end
      |]
    }
    -- blockRam1#
    --   :: ( KnownDomain dom        ARG[0]
    --      , HasCallStack  --       ARG[1]
    --      , Undefined a ) --       ARG[2]
    --   => Clock dom       -- clk,  ARG[3]
    --   -> Enable dom      -- en,   ARG[4]
    --   -> SNat n          -- len,  ARG[5]
    --   -> a               -- init, ARG[6]
    --   -> Signal dom Int  -- rd,   ARG[7]
    --   -> Signal dom Bool -- wren, ARG[8]
    --   -> Signal dom Int  -- wr,   ARG[9]
    --   -> Signal dom a    -- din,  ARG[10]
    --   -> Signal dom a
  , (blackbox P.blockRam1#){
      kind=Declaration
    , template=[I.i|
        // blockRam1 begin,
        ~TYPO ~GENSYM[~RESULT_RAM][1] [0:~LIT[5]-1];
        logic [~SIZE[~TYP[10]]-1:0] ~GENSYM[~RESULT_q][2];
        initial begin
          ~SYM[1] = '{default: ~CONST[6]};
        end~IF ~ISACTIVEENABLE[4] ~THEN
        always @(~IF~ACTIVEEDGE[Rising][0]~THENposedge~ELSEnegedge~FI ~ARG[3]) begin : ~GENSYM[~COMPNAME_blockRam][3]~IF ~VIVADO ~THEN
          if (~ARG[4]) begin
            if (~ARG[8]) begin
              ~SYM[1][~ARG[9]] <= ~TOBV[~ARG[10]][~TYP[10]];
            end
            ~SYM[2] <= ~SYM[1][~ARG[7]];
          end~ELSE
          if (~ARG[8] & ~ARG[4]) begin
            ~SYM[1][~ARG[9]] <= ~TOBV[~ARG[10]][~TYP[10]];
          end
          if (~ARG[4]) begin
            ~SYM[2] <= ~SYM[1][~ARG[7]];
          end~FI
        end~ELSE
        always @(~IF~ACTIVEEDGE[Rising][0]~THENposedge~ELSEnegedge~FI ~ARG[3]) begin : ~SYM[3]
          if (~ARG[8]) begin
            ~SYM[1][~ARG[9]] <= ~TOBV[~ARG[10]][~TYP[10]];
          end
          ~SYM[2] <= ~SYM[1][~ARG[7]];
        end~FI
        assign ~RESULT = ~FROMBV[~SYM[2]][~TYP[10]];
        // blockRam1 end
      |]
  ]