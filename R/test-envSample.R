# test for envSample in dismo

context("envSample")

test_that("test if envSample filter the data as expected", {
            filters<- list (c(1:10), c(1:10))
            coord<- data.frame (c(1:10), c(1:10))
            filt<- envSample (coord, filters=filters, 
                                         res=list (1, 1), 
                                         do.plot=FALSE)
            expect_that(dim (filt)[1], equals (10))
            filt2<- envSample (coord, filters=filters, 
                              res=list (2, 2), 
                              do.plot=FALSE)
            expect_that(dim (filt2)[1], equals (5))
            filt3<- envSample (coord, filters=filters, 
                               res=list (3, 3), 
                               do.plot=FALSE)
            expect_that(dim (filt3)[1], equals (4))
            filt4<- envSample (coord, filters=filters, 
                               res=list (5, 5), 
                               do.plot=FALSE)
            expect_that(dim (filt4)[1], equals (2))
            })