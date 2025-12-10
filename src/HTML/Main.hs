module HTML.Main where

import qualified Data.Text as T
import           Lucid

import           Storage.Types

bookPage :: Html ()
bookPage = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "UTF-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "OnLib"

    link_ [rel_ "stylesheet", href_ "css/bootstrap.min.css"]
    link_ [rel_ "stylesheet", href_ "css/main.css"]
    script_ [type_ "module", src_ "js/app/init.js"] T.empty

  body_ [id_ "top", class_ "d-flex flex-column min-vh-100", style_ "background-color: #fdfdf7;"] $ do
    siteHeader
    mainContent
    siteFooter


siteHeader :: Html ()
siteHeader =
  header_ [class_ "bg-primary mb-4 shadow-sm"] $
    nav_ [class_ "navbar navbar-expand-lg navbar-dark container"] $ do
      a_ [class_ "navbar-brand fw-semibold", href_ "#top"] "📚 Книжный Мир"

      ul_ [class_ "navbar-nav ms-auto mb-2 mb-lg-0"] $ do
        li_ [class_ "nav-item"] $
          a_ [class_ "nav-link", href_ "#books"] "Каталог"
        li_ [class_ "nav-item"] $
          a_ [class_ "nav-link", href_ "#about"] "О нас"
        li_ [class_ "nav-item"] $
          a_ [class_ "nav-link", href_ "#contact"] "Контакты"


mainContent :: Html ()
mainContent =
  main_ [class_ "container mb-5 flex-grow-1"] $ do

    section_ [id_ "books", class_ "mb-5"] $ do
      h2_ [class_ "h2 text-center mb-4"] "Новинки литературы"
      div_ [id_ "book-list", class_ "row g-4"] $ return ()

    section_ [id_ "about", class_ "mb-5"] $ do
      h2_ [class_ "h2 text-center mb-4"] "О нашей библиотеке"

      div_ [class_ "row justify-content-center"] $ do
        div_ [class_ "col-lg-8"] $ do
          p_ [class_ "lead text-center mb-3"]
            "Мы работаем с 2010 года, предлагая лучшие книги по программированию."
          p_ "Наши преимущества:"
          ul_ [class_ "list-group list-group-flush bg-transparent"] $ do
            li_ [class_ "list-group-item bg-transparent text-light border-secondary"] "Бесплатная доставка от 3000 ₽"
            li_ [class_ "list-group-item bg-transparent text-light border-secondary"] "Электронные версии в подарок"
            li_ [class_ "list-group-item bg-transparent text-light border-secondary"] "Скидки постоянным клиентам"

    section_ [id_ "contact", class_ "mb-5"] $ do
      h2_ [class_ "h2 text-center mb-4"] "Контактная информация"

      div_ [class_ "row justify-content-center"] $
        div_ [class_ "col-lg-8"] $
          table_ [class_ "table table-dark table-striped table-bordered align-middle"] $ do
            thead_ $
              tr_ $ do
                th_ [class_ "text-center"] "Тип связи"
                th_ [class_ "text-center"] "Данные"
            tbody_ $ do
              tr_ $ do
                td_ "Email"
                td_ "books@example.com"
              tr_ $ do
                td_ "Телефон"
                td_ "+7 (495) 123-45-67"
              tr_ $ do
                td_ "Адрес"
                td_ "г. Москва, ул. Программистов, д. 15"


siteFooter :: Html ()
siteFooter =
  footer_ [class_ "mt-auto py-3 bg-black border-top border-secondary"] $
    div_ [class_ "container text-center text-secondary small"] $ do
      p_ "© 2025 Книжный Мир. Все права защищены."
      p_ $ do
        "Сайт разработан с использованием "
        a_ [class_ "link-light text-decoration-underline", href_ "https://haskell.org"] "Haskell"
        " и библиотеки "
        code_ "lucid"


dbTest :: [FullBook] -> Html ()
dbTest books = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "UTF-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "test"
  
  body_ $ do
    pre_ $ mapM_ (\fb -> toHtml (fullBookLine fb <> "\n")) books
  where
    fullBookLine fb = T.pack $ show fb