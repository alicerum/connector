module Main where
  import qualified Test.HUnit as HU
  import NGram
  main = do
     putStrLn $ showHosts $ findHostNames "other" hosts
     HU.assertBool "Should match second one" $ "other.host.com" `elem` findHostNames "other" hosts
     putStrLn $ showHosts $ findHostNames "some" hosts
     HU.assertBool "Should match first one" $ "some.host.com" `elem` findHostNames "some" hosts
     putStrLn $ showHosts $ findHostNames "strane" hosts
     HU.assertBool "Should match third one by alias"  $ "hosted.host.com" `elem` findHostNames "strange" hosts

  hosts = [
    createHost ["some.host.com", "some.host.alias"],
    createHost ["other.host.com", "other.host.alias"],
    createHost ["hosted.host.com", "strangealias.host.alias"]]

  findHostNames query hosts = names (head $ findCloseHosts query hosts)

