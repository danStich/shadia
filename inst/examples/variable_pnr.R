# Variable upstream and downstream passage at 
# each dam in increments of 0.10, with timing of 1 d:
# -----------------------------------------------------------------------

\dontrun{
 penobscotRiverModel(nRuns = 10000,
  upstream = list(milford = seq(0, 1, 0.05),
                  howland = seq(0, 1, 0.05),
                  westEnfield = seq(0, 1, 0.05),
                  brownsMill = seq(0, 1, 0.05),
                  moosehead = seq(0, 1, 0.05),
                  guilford = seq(0, 1, 0.05),
                  weldon = seq(0, 1, 0.05)
                  ),
  downstream = list(stillwater = seq(0, 1, 0.05),
                    orono = seq(0, 1, 0.05),
                    milford = seq(0, 1, 0.05),
                    howland = seq(0, 1, 0.05),
                    westEnfield = seq(0, 1, 0.05),
                    brownsMill = seq(0, 1, 0.05),
                    moosehead = seq(0, 1, 0.05),
                    guilford = seq(0, 1, 0.05),
                    weldon = seq(0, 1, 0.05)
                    ),
  watershed = FALSE
 ) 
}
