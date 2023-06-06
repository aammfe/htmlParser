{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ParserSpec (spec) where



import Test.Hspec (describe, it, shouldBe, Spec )
import Parser (parseHtml)
import Data.Either  (isRight)
import Html



spec :: Spec
spec = do
  describe "Parsing Tests" $ do
    it "Parse just a big HTML successfully" $ do
      let r = isRight . parseHtml $ justABigHtml
      r `shouldBe` True
    
    it "just a script Tag successfully" $ do
      parseHtml scriptTag `shouldBe` Right [Script [] (TextContent "window.dataLayer = window.dataLayer || [];\n  function gtag(){dataLayer.push(arguments);}\n  gtag('js', new Date());\n\n  gtag('config', 'G-Q3H025ZKLN');\n")]
   
    it "empty script Tag successfully" $ do
      let r = parseHtml . ValidHtml $ "<script></script>" 
      r `shouldBe` Right [Script [] . TextContent $ ""]
   
    it "h1 Tag successfully" $ do
      let r = parseHtml . ValidHtml $ "<h1>content</h1>" 
      r `shouldBe` Right [ManuallyClosingTag (TagName "h1") [] [JustText (TextContent "content")]]


scriptTag :: ValidHtml
scriptTag = ValidHtml "<script>\n  window.dataLayer = window.dataLayer || [];\n  function gtag(){dataLayer.push(arguments);}\n  gtag('js', new Date());\n\n  gtag('config', 'G-Q3H025ZKLN');\n</script>"


justABigHtml :: ValidHtml
justABigHtml = ValidHtml "<html lang=\"en\"><head>\n<meta charset=\"utf-8\">\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n<meta name=\"keywords\" content=\"free html codes, list of html code, web page code, copy and paste, myspace, text, color, background, layout, generator, font, picture, images, photo, music, video, kodlar, kodlari, codici, kode, kodes\">\n<meta name=\"Description\" content=\"Need some HTML code? Check out this list of free HTML Codes. Just copy/paste them into your website or blog!\">\n<link rel=\"canonical\" href=\"https://www.quackit.com/html/codes/\">\n<title>HTML Codes</title>\n<script src=\"//ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js\"></script>\n<link rel=\"shortcut icon\" href=\"https://www.quackit.com/pix/favicon96.png\">\n<link rel=\"apple-touch-icon\" href=\"https://www.quackit.com/pix/apple-touch-icon.png\">\n<link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">\n<link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin=\"\">\n<link href=\"https://fonts.googleapis.com/css2?family=Lato:ital,wght@0,400;0,700;1,400\&amp;display=swap\" rel=\"stylesheet\">\n<link href=\"/common/css/master.43.min.css\" rel=\"stylesheet\">\n\n<script async=\"\" src=\"https://www.googletagmanager.com/gtag/js?id=G-Q3H025ZKLN\"></script>\n<script>\n  window.dataLayer = window.dataLayer || [];\n  function gtag(){dataLayer.push(arguments);}\n  gtag('js', new Date());\n\n  gtag('config', 'G-Q3H025ZKLN');\n</script>\n</head>\n<body data-new-gr-c-s-check-loaded=\"14.1111.0\" data-gr-ext-installed=\"\" cz-shortcut-listen=\"true\">\n<header class=\"site-header\">\n<div class=\"site-header-base\">\n<div class=\"site-logo\">\n<a title=\"Quackit Homepage\" target=\"_top\" href=\"https://www.quackit.com/\"><img src=\"/pix/quackit_logo_watermark.png\" width=\"87\" height=\"33\" alt=\"Quackit Logo\"></a>\n</div>\n<button id=\"site-nav-toggler\" class=\"site-nav-toggler\" aria-expanded=\"false\" aria-controls=\"site-nav\">\n<span class=\"sr-only\">Toggle navigation</span>\n☰\n</button>\n</div>\n<nav id=\"site-nav\" class=\"site-nav\">\n<div class=\"site-links\">\n<ul>\n<li><a href=\"https://www.quackit.com\"><i class=\"fa fa-home\"></i> <span class=\"sr-only\">Home</span></a></li>\n<li><a href=\"https://www.quackit.com/html/\">HTML</a></li>\n<li><a href=\"https://www.quackit.com/css/\">CSS</a></li>\n<li><a href=\"https://www.quackit.com/scripting/\">Scripting</a></li>\n<li><a href=\"https://www.quackit.com/database/\">Database</a></li>\n</ul>\n</div>\n<div class=\"site-search-top\">\n<form action=\"/search/\" id=\"cse-search-box-bottom\" class=\"site-search\">\n<div>\n<input type=\"hidden\" name=\"cx\" value=\"partner-pub-6331358926293806:98x0fk-bbgi\">\n<input type=\"hidden\" name=\"cof\" value=\"FORID:10\">\n<input type=\"hidden\" name=\"ie\" value=\"ISO-8859-1\">\n<input type=\"text\" name=\"q\" size=\"20\" class=\"site-search-input\" placeholder=\"\" style=\"background: url(\&quot;https://www.google.com/cse/static/images/1x/en/branding.png\&quot;) left 9px top 50% no-repeat rgb(255, 255, 255);\">\n<button type=\"submit\" name=\"sa\" class=\"site-search-button\"><i class=\"fa fa-search\"></i></button>\n</div>\n<input name=\"siteurl\" value=\"www.quackit.com/html/codes/\" type=\"hidden\"><input name=\"ref\" value=\"www.mockplus.com/\" type=\"hidden\"><input name=\"ss\" value=\"\" type=\"hidden\"></form>\n</div>\n</nav>\n</header>\n<div class=\"main\">\n<article class=\"content\">\n<h1 class=\"page-title\">HTML Codes</h1>\n<div class=\"ad ad-top\">\n<script async=\"\" src=\"//pagead2.googlesyndication.com/pagead/js/adsbygoogle.js\"></script>\n\n<ins class=\"adsbygoogle responsive-top\" style=\"display:block\" data-ad-client=\"ca-pub-6331358926293806\" data-ad-slot=\"2095624553\" data-ad-format=\"auto\"></ins>\n<script>\n\t\t(adsbygoogle = window.adsbygoogle || []).push({});\n\t\t</script>\n</div>\n<div class=\"cards\">\n<article class=\"card card-unbordered\">\n<a href=\"/html/codes/html_background_codes.cfm\">\n<img src=\"/pix/html/codes/background_codes.png\" alt=\"Background thumbnail\">\n</a>\n<div class=\"card-body\">\n<h2><a href=\"/html/codes/html_background_codes.cfm\">Background Codes</a></h2>\n<ul class=\"list-unstyled\">\n<li><a href=\"/html/codes/html_background_codes.cfm\">HTML Background Codes</a></li>\n<li><a href=\"/html/codes/html_background_image_codes.cfm\">Background Image Codes</a></li>\n<li><a href=\"/html/codes/html_stretch_background_image.cfm\">Stretch Background Image</a></li>\n<li><a href=\"/html/codes/html_background_music_codes.cfm\">Background Music Codes</a></li>\n<li><a href=\"/html/codes/html_fixed_background.cfm\">Fixed Background Code</a></li>\n</ul>\n</div>\n</article>\n<article class=\"card card-unbordered\">\n<a href=\"/html/codes/html_picture_codes.cfm\">\n<img src=\"/pix/html/codes/image_codes.png\" alt=\"Image code thumbnail\">\n</a>\n<div class=\"card-body\">\n<h2><a href=\"/html/codes/html_picture_codes.cfm\">Image Codes</a></h2>\n<ul class=\"list-unstyled\">\n<li><a href=\"/html/codes/html_picture_codes.cfm\">HTML Image Code</a></li>\n<li><a href=\"/html/codes/html_image_borders.cfm\">HTML Image Borders</a></li>\n<li><a href=\"/html/codes/html_image_link_code.cfm\">HTML Image Link Codes</a></li>\n</ul>\n</div>\n</article>\n<article class=\"card card-unbordered\">\n<a href=\"/html/codes/color/\">\n<img src=\"/pix/stock/css_colors_2.png\" alt=\"Color thumbnail\">\n</a>\n<div class=\"card-body\">\n<h2><a href=\"/html/codes/color/\">Color Codes</a></h2>\n<ul class=\"list-unstyled\">\n<li><a href=\"/html/codes/color/\">HTML Color</a></li>\n<li><a href=\"/html/html_color_codes.cfm\">HTML Color Codes</a></li>\n<li><a href=\"/html/codes/color/color_names.cfm\">HTML Color Names</a></li>\n<li><a href=\"/html/codes/html_text_color_code.cfm\">Text Color</a></li>\n<li><a href=\"/html/codes/html_background_codes.cfm\">Background Color Codes</a></li>\n<li><a href=\"/html/codes/comment_box_colors.cfm\">Comment Box Colors</a></li>\n<li><a href=\"/html/codes/scroll_box_color.cfm\">Scroll Box Color</a></li>\n</ul>\n</div>\n</article>\n<article class=\"card card-unbordered\">\n<a href=\"/html/codes/text/\">\n<img src=\"/pix/html/codes/text_codes.png\" alt=\"Text thumbnail\">\n</a>\n<div class=\"card-body\">\n<h2><a href=\"/html/codes/text/\">Text Codes</a></h2>\n<ul class=\"list-unstyled\">\n<li><a href=\"/html/codes/text/\">HTML Text</a></li>\n<li><a href=\"/html/codes/bold/\">HTML Bold</a></li>\n<li><a href=\"/html/html_font_code.cfm\">HTML Font Codes</a></li>\n<li><a href=\"/html/codes/html_font_color_code.cfm\">HTML Font Color Codes</a></li>\n<li><a href=\"/html/codes/html_font_size_code.cfm\">HTML Font Size Codes</a></li>\n<li><a href=\"/html/codes/html_font_style_code.cfm\">HTML Font Style Codes</a></li>\n<li><a href=\"/html/codes/html_text_box_code.cfm\">HTML Text Box Codes</a></li>\n<li><a href=\"/html/codes/html_text_color_code.cfm\">HTML Text Color Codes</a></li>\n</ul>\n</div>\n</article>\n<article class=\"card card-unbordered\">\n<a href=\"/html/codes/tables/\">\n<img src=\"/pix/html/codes/table_codes.png\" alt=\"Table thumbnail\">\n</a>\n<div class=\"card-body\">\n<h2><a href=\"/html/codes/tables/\">Table Codes</a></h2>\n<ul class=\"list-unstyled\">\n<li><a href=\"/html/codes/tables/\">HTML Tables</a></li>\n<li><a href=\"/html/codes/html_table_code.cfm\">HTML Table Codes</a></li>\n<li><a href=\"/html/codes/tables/html_table_background.cfm\">HTML Table Background</a></li>\n<li><a href=\"/html/codes/tables/html_table_background_color.cfm\">Table Background Color</a></li>\n<li><a href=\"/html/codes/tables/html_table_border.cfm\">HTML Table Border</a></li>\n<li><a href=\"/html/codes/tables/html_table_color.cfm\">HTML Table Color</a></li>\n<li><a href=\"/html/codes/tables/html_table_text.cfm\">HTML Table Text</a></li>\n</ul>\n</div>\n</article>\n<article class=\"card card-unbordered\">\n<a href=\"/html/codes/link/\">\n<img src=\"/pix/html/codes/link_codes.png\" alt=\"Link thumbnail\">\n</a>\n<div class=\"card-body\">\n<h2>HTML Links</h2>\n<ul class=\"list-unstyled\">\n<li><a href=\"/html/codes/link/\">Basic Link</a></li>\n<li><a href=\"/html/codes/html_link_code.cfm\">HTML Link Codes</a></li>\n<li><a href=\"/html/codes/html_open_link_in_new_window.cfm\">Open Link in New Window</a></li>\n<li><a href=\"/html/codes/html_popup_window_code.cfm\">Pop up Windows</a></li>\n<li><a href=\"/html/codes/html_image_link_code.cfm\">HTML Image Link</a></li>\n<li><a href=\"/html/codes/html_email_code.cfm\">HTML Email Link</a></li>\n<li><a href=\"/html/tutorial/html_links.cfm\">About HTML Links</a></li>\n<li><a href=\"/css/css_hyperlinks.cfm\">CSS Hover Links</a></li>\n</ul>\n</div>\n</article>\n<article class=\"card card-unbordered\">\n<a href=\"/html/codes/html_scroll_box.cfm\">\n<img src=\"/pix/html/codes/scrollbox_codes.png\" alt=\"Scrollbox thumbnail\">\n</a>\n<div class=\"card-body\">\n<h2><a href=\"/html/codes/html_scroll_box.cfm\">Scroll Boxes</a></h2>\n<ul class=\"list-unstyled\">\n<li><a href=\"/html/codes/html_scroll_box.cfm\">Scroll Box</a></li>\n<li><a href=\"/html/codes/scroll_box_color.cfm\">Colored Scroll Box</a></li>\n<li><a href=\"/html/codes/scroll_box_border.cfm\">Scroll Box with Borders</a></li>\n<li><a href=\"/html/codes/picture_scroll_box.cfm\">Picture Scroll Boxes</a></li>\n<li><a href=\"/html/codes/horizontal_scroll.cfm\">Horizontal Scroll</a></li>\n<li><a href=\"/html/codes/vertical_scroll.cfm\">Vertical Scroll</a></li>\n<li><a href=\"/html/codes/hide_scrollbar.cfm\">Hide Scrollbars</a></li>\n<li><a href=\"/html/codes/div_scroll_position.cfm\">Div Scroll Position</a></li>\n</ul>\n</div>\n</article>\n<article class=\"card card-unbordered\">\n<a href=\"/html/codes/comment_box_code.cfm\">\n<img src=\"/pix/html/codes/commentbox_codes.png\" alt=\"Comment box thumbnail\">\n</a>\n<div class=\"card-body\">\n<h2><a href=\"/html/codes/comment_box_code.cfm\">Comment Boxes</a></h2>\n<ul class=\"list-unstyled\">\n<li><a href=\"/html/codes/comment_box_code.cfm\">Comment Box Codes</a></li>\n<li><a href=\"/html/codes/comment_box_borders.cfm\">Comment Box Borders</a></li>\n<li><a href=\"/html/codes/comment_box_colors.cfm\">Comment Box Colors</a></li>\n<li><a href=\"/html/codes/comment_box_pictures.cfm\">Comment Box Pictures</a></li>\n<li><a href=\"/html/codes/add_comments_to_website.cfm\">Add Comments to Website</a></li>\n<li><a href=\"/html/codes/html_scrollbars.cfm\">HTML Scrollbars</a></li>\n<li><a href=\"/html/codes/hide_scrollbar.cfm\">Hide Scrollbars</a></li>\n</ul>\n</div>\n</article>\n<article class=\"card card-unbordered\">\n<a href=\"/css/codes/marquees/\">\n<img src=\"/pix/html/codes/marquee_codes.png\" alt=\"Marquee thumbnail\">\n</a>\n<div class=\"card-body\">\n<h2><a href=\"/css/codes/marquees/\">Marquees</a></h2>\n<ul class=\"list-unstyled\">\n<li><a href=\"/css/codes/marquees/\">CSS Marquee Codes</a></li>\n<li><a href=\"/html/codes/html_marquee_code.cfm\">HTML Marquee Codes</a></li>\n<li><a href=\"/html/codes/scrolling_text.cfm\">Scrolling Text</a></li>\n<li><a href=\"/html/codes/scrolling_images.cfm\">Scrolling Images</a></li>\n<li><a href=\"/html/codes/stop_marquee.cfm\">HTML Stop Marquee</a></li>\n<li><a href=\"/html/codes/slow_down_marquee.cfm\">Slow Down \&amp; Speed Up Marquee</a></li>\n<li><a href=\"/javascript/codes/javascript_scroll.cfm\">Scrolling Marquee (JavaScript version)</a></li>\n</ul>\n</div>\n</article>\n<article class=\"card card-unbordered\">\n<img src=\"/pix/html/codes/window_codes.png\" alt=\"Window thumbnail\">\n<div class=\"card-body\">\n<h2>Window Codes</h2>\n<ul class=\"list-unstyled\">\n<li><a href=\"/html/codes/html_close_window_code.cfm\">Close Window Codes</a></li>\n<li><a href=\"/html/codes/html_open_link_in_new_window.cfm\">Open Link in New Window</a></li>\n<li><a href=\"/html/codes/html_popup_window_code.cfm\">Popup Window Codes</a></li>\n<li><a href=\"/html/codes/html_scrollbars.cfm\">HTML Scrollbars</a></li>\n</ul>\n</div>\n</article>\n<article class=\"card card-unbordered\">\n<img src=\"/pix/html/codes/music_codes.png\" alt=\"Music thumbnail\">\n<div class=\"card-body\">\n<h2>Music/Video Codes</h2>\n<ul class=\"list-unstyled\">\n<li><a href=\"/html/codes/create_music_playlist.cfm\">Create a Music Playlist</a></li>\n<li><a href=\"/html/howto/how_to_create_a_music_playlist_in_wordpress.cfm\">Create a Music Playlist in WordPress</a></li>\n<li><a href=\"/html/html_music_code.cfm\">HTML Music Codes</a></li>\n<li><a href=\"/html/codes/html_background_music_codes.cfm\">Background Music Codes</a></li>\n<li><a href=\"/html/codes/html_video_codes.cfm\">HTML Video Codes</a></li>\n</ul>\n</div>\n</article>\n<article class=\"card card-unbordered\">\n<a href=\"/html/codes/html_form_code.cfm\">\n<img src=\"/pix/html/codes/form_codes.png\" alt=\"Form thumbnail\">\n</a>\n<div class=\"card-body\">\n<h2><a href=\"/html/codes/html_form_code.cfm\">Forms</a></h2>\n<ul class=\"list-unstyled\">\n<li><a href=\"/html/codes/html_form_code.cfm\">HTML Form Codes</a></li>\n<li><a href=\"/html/codes/html_text_input.cfm\">HTML Text Input</a></li>\n<li><a href=\"/html/codes/html_form_to_email.cfm\">HTML Form To Email</a></li>\n<li><a href=\"/html/codes/mailto_form.cfm\">Mailto Form</a></li>\n<li><a href=\"/html/codes/html_button.cfm\">HTML Button</a></li>\n<li><a href=\"/html/codes/html_radio_button.cfm\">HTML Radio Button</a></li>\n</ul>\n</div>\n</article>\n<article class=\"card card-unbordered\">\n<img src=\"/pix/html/codes/frames_codes.png\" alt=\"Frames thumbnail\">\n<div class=\"card-body\">\n<h2>Frames</h2>\n<ul class=\"list-unstyled\">\n<li><a href=\"/html/codes/html_frames_code.cfm\">HTML Frames Codes</a></li>\n<li><a href=\"/html/templates/frames/\">HTML Frames Templates</a></li>\n<li><a href=\"/html/examples/frames/\">HTML Frames Examples</a></li>\n</ul>\n</div>\n</article>\n<article class=\"card card-unbordered\">\n<img src=\"/pix/html/codes/more_codes.png\" alt=\"Codes thumbnail\">\n<div class=\"card-body\">\n<h2>More HTML Codes</h2>\n<ul class=\"list-unstyled\">\n<li><a title=\"Create editable content\" href=\"/html/codes/contenteditable.cfm\">Contenteditable</a></li>\n<li><a href=\"/html/codes/html_borders.cfm\">HTML Border Codes</a></li>\n<li><a href=\"/html/codes/html_code_for_colors.cfm\">HTML Code for Colors</a></li>\n<li><a href=\"/html/codes/html_comments_code.cfm\">HTML Comments Codes</a></li>\n<li><a href=\"/html/codes/html_cursor_code.cfm\">HTML Cursor Codes</a></li>\n<li><a href=\"/html/codes/list/\">HTML List Code</a></li>\n<li><a href=\"/html/codes/meta_refresh.cfm\">HTML Meta Refresh Code</a></li>\n<li><a href=\"/html/html_redirect.cfm\">HTML Redirect Codes</a></li>\n<li><a href=\"/javascript/codes/timed_javascript_redirect.cfm\">Timed redirect</a></li>\n</ul>\n</div>\n</article>\n<article class=\"card card-unbordered\">\n<img src=\"/pix/html/codes/html_generators.png\" alt=\"HTML generators thumbnail\">\n<div class=\"card-body\">\n<h2>HTML Generators</h2>\n<ul class=\"list-unstyled\">\n<li><a href=\"/html/online-html-editor/\">Online HTML Editor</a></li>\n<li><a href=\"/html/html_generators/html_code_generator.cfm\">HTML Code Generator</a></li>\n<li><a href=\"/html/html_table_generator.cfm\">HTML Table Generator</a></li>\n<li><a href=\"/html/html_generators/html_marquee_generator.cfm\">Marquee Generator</a></li>\n<li><a href=\"/html/html_generators/music_code_generator.cfm\">Music Code Generator</a></li>\n<li><a href=\"/html/html_generators/html_text_generator.cfm\">HTML Text Generator</a></li>\n<li><a href=\"/html/html_generators/html_text_box_generator.cfm\">Text Box Generator</a></li>\n</ul>\n</div>\n</article>\n</div>\n<h2>HTML Reference</h2>\n<ul class=\"steps\">\n<li>\n<figure>\n<a href=\"https://www.quackit.com/html/tutorial/\"><img class=\"steps\" src=\"/pix/stock/html_tutorial_500x200_2.gif\" alt=\"Screenshot of HTML code\"></a>\n<figcaption>\n<h3><a href=\"https://www.quackit.com/html/tutorial/\">HTML Tutorial</a></h3>\n<p>Free HTML tutorial that explains how to code in HTML.</p>\n<p>This tutorial explains what HTML elements and attributes are, and how to use them.</p>\n<p>I explain the basics, such as what you need in order to write HTML and how to create your first web page.</p>\n<p>I then cover other HTML topics including tables, adding color, images, forms, image maps, iframes, meta tags, and more.</p>\n<p>I also explain the difference between HTML and CSS (and when to use each one).</p>\n<a class=\"btn btn-default\" href=\"https://www.quackit.com/html/tutorial/\">Go to HTML Tutorial</a>\n</figcaption>\n</figure>\n</li>\n<li>\n<figure>\n<a href=\"https://www.quackit.com/html/tags/\"><img class=\"steps\" src=\"/pix/html/tags/html_tags.gif\" alt=\"Screenshot of HTML tags\"></a>\n<figcaption>\n<h3><a href=\"https://www.quackit.com/html/tags/\">HTML Tags</a></h3>\n<p>Full list of all HTML elements.</p>\n<p>This is an alphabetical list of HTML elements, linking to a full page of details for each element.</p>\n<p>All elements are based on the official HTML5 specification, and include usage notes, full attribute list, as well as links to the various specifications for each element (i.e. HTML4 spec, HTML5 spec, WHATWG spec).</p>\n<a class=\"btn btn-default\" href=\"https://www.quackit.com/html/tags/\">Go to HTML Tags</a>\n</figcaption>\n</figure>\n</li>\n<li>\n<figure>\n<a href=\"https://www.quackit.com/css/properties/\"><img class=\"steps\" src=\"/pix/css/properties/css_properties.gif\" alt=\"Screenshot of CSS Properties\"></a>\n<figcaption>\n<h3><a href=\"https://www.quackit.com/css/properties/\">CSS Properties</a></h3>\n<p>Full list of CSS properties.</p>\n<p>Alphabetical list of CSS properties as per the W3C specifications. </p>\n<p>CSS stands for Cascading Style Sheets. CSS is the standard way to style web pages.</p>\n<p>You can use CSS to set the style for a whole website in one place. CSS allows you to set colors, fonts, widths, heights, margins, padding, and much more.</p>\n<a class=\"btn btn-default\" href=\"https://www.quackit.com/css/properties/\">Go to CSS Properties</a>\n</figcaption>\n</figure>\n</li>\n</ul>\n<div class=\"ad ad-bottom\">\n<script async=\"\" src=\"//pagead2.googlesyndication.com/pagead/js/adsbygoogle.js\"></script>\n\n<ins class=\"adsbygoogle responsive-bottom\" style=\"display:block\" data-ad-client=\"ca-pub-6331358926293806\" data-ad-slot=\"4873379751\" data-ad-format=\"auto\"></ins>\n<script>\n\t\t(adsbygoogle = window.adsbygoogle || []).push({});\n\t\t</script>\n</div>\n</article>\n<div class=\"sidebar\">\n<nav>\n<ul>\n<li class=\"selected\">\n<h3><a href=\"https://www.quackit.com/html/codes/\">HTML Codes</a></h3>\n<ul>\n<li><a href=\"/html/codes/html_background_codes.cfm\" title=\"Set background properties on any HTML element.\">HTML Background Code</a></li>\n<li><a href=\"/html/codes/bold/\" title=\"Specify bold text in your HTML code.\">HTML Bold</a></li>\n<li><a href=\"/html/codes/color/\" title=\"Add color to your HTML code.\">HTML Color</a></li>\n<li><a href=\"/html/html_color_codes.cfm\" title=\"Hexadecimal color codes\">HTML Color Codes</a></li>\n<li><a href=\"/html/codes/comment_box_code.cfm\" title=\"Create a comment box within your HTML codes.\">HTML Comment Box Code</a></li>\n<li><a href=\"/html/codes/html_scroll_box.cfm\" title=\"Create a scroll box within your HTML codes.\">HTML Scrollbox Code</a></li>\n<li><a href=\"/html/codes/tables/\" title=\"Add an HTML table to your web page or blog.\">HTML Tables</a></li>\n<li><a href=\"/html/codes/text/\" title=\"Specify font family, size, color and more within your HTML code.\">HTML Text Code</a></li>\n<li><a href=\"/html/codes/html_picture_codes.cfm\" title=\"Embed pictures within your HTML code.\">HTML Image Code</a></li>\n<li><a href=\"/html/codes/link/\" title=\"Create hyperlinks between web pages and other documents.\">HTML Link Code</a></li>\n<li><a href=\"/html/codes/html_marquee_code.cfm\">HTML Marquee Code</a></li>\n<li><a href=\"/html/html_music_code.cfm\">HTML Music Code</a></li>\n<li><a href=\"/html/codes/html_video_codes.cfm\">HTML Video Code</a></li>\n<li><a href=\"/html/codes/html_form_code.cfm\">HTML Form Code</a></li>\n<li><a href=\"/html/codes/html_frames_code.cfm\">HTML Frames Code</a></li>\n<li><a href=\"/html/html_special_characters.cfm\" title=\"ISO-8859-1\">HTML Entities</a></li>\n<li><a href=\"/html/examples/\" title=\"Copy/paste examples\">HTML Examples</a></li>\n<li><a href=\"/html/html_help.cfm\" title=\"HTML help with tables, forms, fonts, color, image maps, hyperlinks and more.\">HTML Help/Cheat Sheet</a></li>\n<li><a href=\"/html/templates/\" title=\"Thousands of pre-built websites\">HTML Templates</a></li>\n<li class=\"selected\"><a style=\"padding-left:60px;font-style:italic;\" href=\"/html/codes/\">More Codes...</a></li>\n</ul>\n</li>\n<li>\n<h3><a href=\"https://www.quackit.com/html/\">HTML Reference</a></h3>\n<ul>\n<li><a href=\"https://www.quackit.com/html/tags/\" title=\"Alphabetical list of all HTML tags\">HTML Tags</a></li>\n<li class=\"selected\"><a href=\"https://www.quackit.com/html/codes/\" title=\"\">HTML Codes</a></li>\n<li><a href=\"https://www.quackit.com/html/templates/\" title=\"\">HTML Templates</a></li>\n<li><a href=\"https://www.quackit.com/html/html_editors/\">HTML Editors</a></li>\n<li><a href=\"https://www.quackit.com/html/tutorial/\" title=\"\">HTML Tutorial</a></li>\n<li><a href=\"https://www.quackit.com/create-a-website/\" title=\"\">Create a Website</a></li>\n<li><a href=\"/character_sets/\">Character Set Reference</a></li>\n</ul>\n</li>\n</ul>\n</nav>\n<div class=\"ad ad-left\">\n<script async=\"\" src=\"//pagead2.googlesyndication.com/pagead/js/adsbygoogle.js\"></script>\n\n<ins class=\"adsbygoogle\" style=\"display:block\" data-ad-client=\"ca-pub-6331358926293806\" data-ad-slot=\"7187717753\" data-ad-format=\"auto\"></ins>\n<script>\n\t\t(adsbygoogle = window.adsbygoogle || []).push({});\n\t\t</script>\n</div>\n</div>\n<div class=\"ads\">\n<div class=\"ad ad-right\">\n<script async=\"\" src=\"//pagead2.googlesyndication.com/pagead/js/adsbygoogle.js\"></script>\n\n<ins class=\"adsbygoogle\" style=\"display:block\" data-ad-client=\"ca-pub-6331358926293806\" data-ad-slot=\"9157690018\" data-ad-format=\"auto\"></ins>\n<script>\n\t\t(adsbygoogle = window.adsbygoogle || []).push({});\n\t\t</script>\n</div>\n</div>\n</div>\n<div class=\"searchbox-bottom\">\n<form action=\"/search/\" id=\"cse-search-box-bottom\" class=\"site-search\">\n<div>\n<input type=\"hidden\" name=\"cx\" value=\"partner-pub-6331358926293806:npmuvy-i8kk\">\n<input type=\"hidden\" name=\"cof\" value=\"FORID:10\">\n<input type=\"hidden\" name=\"ie\" value=\"ISO-8859-1\">\n<input type=\"text\" name=\"q\" size=\"30\" class=\"site-search-input\">\n<button type=\"submit\" name=\"sa\" class=\"site-search-button\"><i class=\"fa fa-search\"></i></button>\n</div>\n</form>\n<script src=\"//cse.google.com/cse/brand?form=cse-search-box-bottom\&amp;lang=en\"></script>\n</div>\n<footer>\n<p class=\"about\"><a href=\"/\"><i class=\"fa fa-home\"></i> Home</a> | <a href=\"/about.cfm\" rel=\"nofollow\">About</a> | <a href=\"/contact.cfm\" rel=\"nofollow\">Contact</a> | <a href=\"/terms_of_use.cfm\" rel=\"nofollow\">Terms\&nbsp;of\&nbsp;Use</a> | <a href=\"/privacy_policy.cfm\" rel=\"nofollow\">Privacy\&nbsp;Policy</a></p>\n<p>© Copyright 2000 - 2023 Quackit.com \&nbsp;</p>\n</footer>\n<script src=\"/common/js/spectrum/spectrum.js\"></script>\n<script src=\"/common/js/lightbox2-master/dist/js/lightbox.min.js\" charset=\"utf-8\"></script><div id=\"lightboxOverlay\" class=\"lightboxOverlay\" style=\"display: none;\"></div><div id=\"lightbox\" class=\"lightbox\" style=\"display: none;\"><div class=\"lb-outerContainer\"><div class=\"lb-container\"><img class=\"lb-image\" src=\"data:image/gif;base64,R0lGODlhAQABAIAAAP///wAAACH5BAEAAAAALAAAAAABAAEAAAICRAEAOw==\"><div class=\"lb-nav\"><a class=\"lb-prev\" href=\"\"></a><a class=\"lb-next\" href=\"\"></a></div><div class=\"lb-loader\"><a class=\"lb-cancel\"></a></div></div></div><div class=\"lb-dataContainer\"><div class=\"lb-data\"><div class=\"lb-details\"><span class=\"lb-caption\"></span><span class=\"lb-number\"></span></div><div class=\"lb-closeContainer\"><a class=\"lb-close\"></a></div></div></div></div>\n<script>\n        $(document).ready(function(){\n\n          $( \"#site-nav-toggler\" ).click(function() {\n\n            $( \"#site-nav\" ).toggle( \"slow\" );\n\n          });\n\n        });\n        </script>\n<script>\n            $(function(){var a=window.location.href;$(\".sidebar nav a\").each(function(){a==this.href\&\&$(this).closest(\"li\").addClass(\"selected\")})});\n        \n            \n        </script>\n<script defer=\"\" src=\"https://static.cloudflareinsights.com/beacon.min.js/v52afc6f149f6479b8c77fa569edb01181681764108816\" integrity=\"sha512-jGCTpDpBAYDGNYR5ztKt4BQPGef1P0giN6ZGVUi835kFF88FOmmn8jBQWNgrNd8g/Yu421NdgWhwQoaOPFflDw==\" data-cf-beacon=\"{\&quot;rayId\&quot;:\&quot;7d221b366b293df9\&quot;,\&quot;version\&quot;:\&quot;2023.4.0\&quot;,\&quot;r\&quot;:1,\&quot;token\&quot;:\&quot;f95a87206c5140fb90f2612ffd79569d\&quot;,\&quot;si\&quot;:100}\" crossorigin=\"anonymous\"></script>\n\n\n</body><grammarly-desktop-integration data-grammarly-shadow-root=\"true\"></grammarly-desktop-integration></html>"
