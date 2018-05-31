# Watershed implementation with passage efficiency 
# increments of 0.10 and all combinations of upstream 
# and downstream rates with 24-h standard:
# -----------------------------------------------------------------------

\dontrun{
penobscotRiverModel(nRuns = 1,
 upstream = list(milford = seq(0, 1, 0.10),
                 howland = 1,
                 westEnfield = 1,
                 brownsMill = 1,
                 moosehead = 1,
                 guilford = 1,
                 weldon = 1),
 downstream = list(stillwater = seq(0, 1, 0.10),
                   orono = 1,
                   milford = 1,
                   howland = 1,
                   westEnfield = 1,
                   brownsMill = 1,
                   moosehead = 1,
                   guilford = 1,
                   weldon = 1))
}
