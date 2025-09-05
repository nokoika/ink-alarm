module Service.ICal
  ( generateICalText,
  )
where

import App.Monad (AppM)
import qualified Filter
import qualified ICal
import qualified Query
import qualified SplaApi
import Prelude (String, pure)

-- Generate iCal text from query and API data (pure business logic)
generateICalText :: Query.QueryRoot -> SplaApi.Root -> AppM String
generateICalText query SplaApi.Root {result = apiResult} =
  let iCalInput = Filter.createICalInput query apiResult
      iCalText = ICal.buildICalText iCalInput
   in pure iCalText
