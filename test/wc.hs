main = do
  bs <- LBS8.getContents
  print $ length $ LBS8.words bs
