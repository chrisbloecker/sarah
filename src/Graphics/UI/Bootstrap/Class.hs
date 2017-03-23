module Graphics.UI.Bootstrap.Class
  where

newtype Class = Class { unClass :: String }

buildClass :: [Class] -> Class
buildClass = Class . unwords . map unClass

-- a normal button
btn = Class "btn"

-- button sizes
btn_xs = Class "btn-xs"
btn_sm = Class "btn-sm"
btn_md = Class "btn-md"
btn_lg = Class "btn-lg"

-- kinds of buttons
btn_default = Class "btn-default"
btn_primary = Class "btn-primary"
btn_success = Class "btn-success"
btn_info = Class "btn-info"
btn_warning = Class "btn-warning"
btn_danger = Class "btn-danger"
btn_link = Class "btn-link"

-- button shapes
btn_circle = Class "btn-circle"

-- additional styles
btn_no_border = Class "btn-no-border"
