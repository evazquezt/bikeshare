BikeshareStation <- setClass("BikeshareStation",
                             slots=c(name="character",
                                 lat="numeric",
                                 long="numeric",
                                 installDate="POSIXt",
                                 removalDate="POSIXt",
                                 stationId="numeric",
                                 numBikes="numeric"))
