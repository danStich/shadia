# Variable upstream and downstream passage at 
# each dam in increments of 0.10, with timing of 1 d:
# -----------------------------------------------------------------------

\dontrun{
 penobscotRiverModel(nRuns = 10,
  upstream = list(milford = seq(0, 1, 0.10),
                  howland = seq(0, 1, 0.10),
                  westEnfield = seq(0, 1, 0.10),
                  brownsMill = seq(0, 1, 0.10),
                  moosehead = seq(0, 1, 0.10),
                  guilford = seq(0, 1, 0.10),
                  weldon = seq(0, 1, 0.10)
                  ),
  downstream = list(stillwater = seq(0, 1, 0.10),
                    orono = seq(0, 1, 0.10),
                    milford = seq(0, 1, 0.10),
                    howland = seq(0, 1, 0.10),
                    westEnfield = seq(0, 1, 0.10),
                    brownsMill = seq(0, 1, 0.10),
                    moosehead = seq(0, 1, 0.10),
                    guilford = seq(0, 1, 0.10),
                    weldon = seq(0, 1, 0.10)
                    )
 ) 
}
