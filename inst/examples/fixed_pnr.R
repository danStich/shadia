# Fixed upstream and downstream passage at 
# each dam in increments of 0.10, with timing of 1 d:
# -----------------------------------------------------------------------

\dontrun{
 penobscotRiverModel(nRuns = 10,
  upstream = list(milford = 0.85,
                  howland = 0.90,
                  westEnfield = 0.95,
                  brownsMill = 0.70,
                  moosehead = 0.70,
                  guilford = 0.50,
                  weldon = 0.80
                  ),
  downstream = list(stillwater = 0.95,
                    orono = 0.95,
                    milford = 0.90,
                    howland = 0.90,
                    westEnfield = 0.90,
                    brownsMill = 1.00,
                    moosehead = 1.00,
                    guilford = 1.00,
                    weldon = 0.90
                    )
 ) 
}
