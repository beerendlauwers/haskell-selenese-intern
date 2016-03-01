module Test.WebDriver.InternFormatter where

import Test.WebDriver.Selenese
import Data.List (intersperse)

data InternConfiguration = InternConfig { 
 internConfigCredentials :: String, -- "user:pass"
 internConfigSiteUrl :: String,
 internConfigTimeout :: Int,
 internConfigDebug :: Bool
 }

generateInternOutput :: InternConfiguration -> [Selenese] -> String
generateInternOutput c s = 
 unlines [
    "define(function (require) {",
    "\tvar registerSuite = require('intern!object');",
    "\tvar assert = require('intern/chai!assert');",
    "\tvar credentials = " ++ (if length (internConfigCredentials c) > 0 then "'http://" ++ internConfigCredentials c ++ "@'" else "''") ++ ";",
    "\tvar url = '" ++ internConfigSiteUrl c ++ "';",
    "\tvar total_url = credentials + url;",
    "\tvar storedVars = {};",
    concat (generateStoreCode store),
    "\t",
    "\t\tregisterSuite({",
    "\t\t\tname: 'index',",
    "\t\t\t'greeting form': function () {",
    "\t\t\t\treturn this.remote",
    "\t\t\t\t.get(require.toUrl(total_url))",
    "\t\t\t\t.setFindTimeout(" ++ show (internConfigTimeout c) ++ ")",
    concat (if internConfigDebug c then generateDebugCode code else generateTestCode code),
    "\t\t\t\t}",
    "\t\t\t});",
    "});"
 ]
  where code = filter (not.isStoreCommand) s
        store = filter isStoreCommand s
 
padLines :: [String] -> [String]
padLines = intersperse "\n" . map ("\t\t\t\t\t" ++)
 
generateDebugCode :: [Selenese] -> [String]
generateDebugCode s = (padLines . concat) (map out zipped)
 where zipped = zip (map seleneseToDebug s) (map seleneseToChai s)
       out (x,y) = [x,y]

seleneseToDebug :: Selenese -> String
seleneseToDebug s = "/* INTERPRETED AS: " ++ show s ++ " */"

generateStoreCode :: [Selenese] -> [String]
generateStoreCode = padLines . map seleneseToChai . filter isStoreCommand

generateTestCode :: [Selenese] -> [String]
generateTestCode = padLines . map seleneseToChai . filter (not.isStoreCommand)

isStoreCommand :: Selenese -> Bool
isStoreCommand (Store _ _) = True
isStoreCommand (StoreEval _ _) = True
isStoreCommand (StoreValue _ _) = True
isStoreCommand _ = False
 
seleneseToChai :: Selenese -> String
seleneseToChai (Open t) = ".get(require.toUrl('" ++ extractTarget t ++ "'))"
seleneseToChai (Click t) = findTargetToChai t ++ ".click().end()"
seleneseToChai (ClickAndWait t) = findTargetToChai t ++ ".click().end()"
seleneseToChai (Type t v) = findTargetToChai t ++ ".click().type("++ valueToJSStructure v ++ ").end()"
-- | AssertTextPresent Target Value
-- | VerifyTextPresent Target Value
seleneseToChai (VerifyText t v) = findTargetToChai t ++ assertStrictEqual v
seleneseToChai (AssertText t v) = findTargetToChai t ++ assertStrictEqual v
seleneseToChai (WaitForElementPresent t) = findTargetToChai t ++ ".click().end()"
seleneseToChai (VerifyElementPresent t) = findAllTargetsToChai t ++ ".then(function(elems){ assert.operator(elems.length,'>', 0); }).end()"
seleneseToChai (VerifyElementNotPresent t) = findAllTargetsToChai t ++ ".then(function(elems){ assert.lengthOf(elems,0); }).end()"
seleneseToChai (WaitForElementNotPresent t) = waitForTargetToChai t
-- | WaitForTextPresent Target
-- | WaitForTextNotPresent Target
-- | VerifyTextNotPresent Target
-- | WaitForText Target Value
seleneseToChai (WaitForChecked t) = findTargetToChai t ++ ".isEnabled().then(function (isEnabled) { assert.isTrue(isEnabled); }).end()"
seleneseToChai (WaitForNotChecked t) = findTargetToChai t ++ ".isEnabled().then(function (isEnabled) { assert.isFalse(isEnabled); }).end()"
seleneseToChai (Store t v) = "storedVars['" ++ concatMap escapeSingleJSQuote (replacePlaceholder v) ++ "'] = " ++ (valueToJSStructure . toValue . extractTarget) t ++  ";"
--seleneseToChai (Store t v) = ".execute('" ++ concatMap escapeSingleJSQuote ("storedVars['" ++ replacePlaceholder v ++ "'] = " ++ (valueToJSStructure . toValue . extractTarget) t ++  ";") ++ "')"
-- seleneseToChai (StoreEval t v) = "eval" -- TODO: implement.
--  | StoreValue Target Value -- TODO: implement.
--  | StoreAttribute Target Value -- TODO: implement.
-- | GotoIf Target Value -- TODO: implement.
seleneseToChai (SelectFrame t) = ".switchToFrame('" ++ extractTarget t ++ "')"
seleneseToChai (Label s) = "/*" ++ show s ++ "*/"
seleneseToChai (Pause (Identifier p)) =  ".sleep(" ++ p ++ ")"
-- | Echo Target -- Not translated.
seleneseToChai (SetTimeout (Identifier t)) = ".setFindTimeout(" ++ t ++ ")"
seleneseToChai (Select t v) = findTargetToChai t ++ ".findByCssSelector('option[value=" ++ extractValue v ++ "]').click().end().end()"
--seleneseToChai (AssertElementPresent Target) -- TODO: add.
seleneseToChai (Unsupported c t v) = "/* Unsupported: " ++ show c ++ " " ++ show t ++ " " ++ show v ++ "*/"
seleneseToChai x = "/* Unsupported: " ++ show x ++ "*/"

findTargetToChai :: Target -> String
findTargetToChai (Identifier s) = ".findById('" ++ s ++ "')" -- TODO: This actually tries to find by ID first and, failing that, find by name.
findTargetToChai (Id s) = ".findById('" ++ s ++ "')"
findTargetToChai (Name s) = ".findByName('" ++ s ++ "')"
findTargetToChai (XPath s) = ".findByXpath('" ++ concatMap escapeSingleJSQuote s ++ "')"
findTargetToChai (Link s) = ".findByPartialLinkText('" ++ s ++ "')"
findTargetToChai (CSS s) = ".findByCssSelector('" ++ s ++ "')"
findTargetToChai (DOM s) = "Not implemented"

findAllTargetsToChai :: Target -> String
findAllTargetsToChai (Identifier s) = ".findById('" ++ s ++ "')" -- TODO: This actually tries to find by ID first and, failing that, find by name.
findAllTargetsToChai (Id s) = ".findById('" ++ s ++ "')"
findAllTargetsToChai (Name s) = ".findAllByName('" ++ s ++ "')"
findAllTargetsToChai (XPath s) = ".findAllByXpath('" ++ concatMap escapeSingleJSQuote s ++ "')"
findAllTargetsToChai (Link s) = ".findAllByPartialLinkText('" ++ s ++ "')"
findAllTargetsToChai (CSS s) = ".findAllByCssSelector('" ++ s ++ "')"
findAllTargetsToChai (DOM s) = "Not implemented"


waitForTargetToChai :: Target -> String
waitForTargetToChai (Identifier s) = ".waitForDeletedById('" ++ s ++ "')" -- TODO: This actually tries to find by ID first and, failing that, find by name.
waitForTargetToChai (Id s) = ".waitForDeletedById('" ++ s ++ "')"
waitForTargetToChai (Name s) = ".waitForDeletedByName('" ++ s ++ "')"
waitForTargetToChai (XPath s) = ".waitForDeletedByXpath('" ++ s ++ "')"
waitForTargetToChai (Link s) = ".waitForDeletedByLinkText('" ++ s ++ "')"
waitForTargetToChai (CSS s) = ".waitForDeletedByCssSelector('" ++ s ++ "')"
waitForTargetToChai (DOM s) = "Not implemented"

extractTarget :: Target -> String
extractTarget (Identifier s) = s
extractTarget (Id s) = s
extractTarget (Name s) = s
extractTarget (XPath s) = s
extractTarget (Link s) = s
extractTarget (CSS s) = s
extractTarget (DOM s) = s

extractValue :: Value -> String
extractValue (Placeholder s) = s
extractValue (Normal s) = s

escapeSingleJSQuote :: Char -> String
escapeSingleJSQuote c | c == '\'' = "\\'"
                      | otherwise = [c]

replacePlaceholder :: Value -> String
replacePlaceholder (Placeholder s) = "storedVars['" ++ s ++ "']"
replacePlaceholder (Normal s) = s

-- | Placeholders will get replaced with a reference to the StoredVars object, normal values will just get quoted.
valueToJSStructure :: Value -> String
valueToJSStructure v@(Placeholder s) = replacePlaceholder v
valueToJSStructure (Normal s) = "'" ++ s ++ "'"

assertStrictEqual :: Value -> String
assertStrictEqual x = ".getVisibleText().then(function(text){ assert.strictEqual(text, " ++ valueToJSStructure x ++ "); }).end()"

-- Debug
runDebugTest :: String -> IO () 
runDebugTest s = do { s <- getSeleniumFile s; writeFile "output.js" (generateInternOutput debugConfig s) }

debugConfig :: InternConfiguration
debugConfig = InternConfig { 
 internConfigCredentials = "eandis:RPyUuxBdp9GY",
 internConfigSiteUrl = "eandisbeqa.prod.acquia-sites.com/user",
 internConfigTimeout = 3000,
 internConfigDebug = True
 }