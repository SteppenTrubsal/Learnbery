module HTML.Main where

import qualified Data.Text as T
import           Lucid
import           Lucid.Base

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

  body_ [id_ "top", class_ "d-flex flex-column min-vh-100 text-dark", style_ "background-color:#fdfdf7;"] $ do
    siteHeader
    mainContent
    siteFooter

siteHeader :: Html ()
siteHeader =
  header_ [id_ "site-header", class_ "site-header"] $
    nav_ [class_ "navbar container py-3"] $ do
      a_ [class_ "navbar-brand fw-semibold", href_ "#home", makeAttribute "data-panel" "home"] "📚 Learnbery"
      ul_ [class_ "navbar-nav ms-auto flex-row gap-3"] $ do
        li_ [class_ "nav-item"] $
          a_ [class_ "nav-link", href_ "#catalog", makeAttribute "data-panel" "catalog"] "Каталог"
        li_ [class_ "nav-item"] $
          a_ [class_ "nav-link", href_ "#about", makeAttribute "data-panel" "about"] "О нас"
        li_ [class_ "nav-item"] $
          a_ [class_ "nav-link", href_ "#contact", makeAttribute "data-panel" "contact"] "Контакты"

mainContent :: Html ()
mainContent =
  main_ [class_ "container mb-5 flex-grow-1"] $ do

    section_ [id_ "home", class_ "app-panel is-active"] $ do
      h1_ [class_ "display-6 mb-3"] "Главная"
      p_ [class_ "lead"] "Здесь будет главная страница (пока placeholder)."

    section_ [id_ "catalog", class_ "app-panel mb-5"] $ do
      h2_ [class_ "h2 text-center mb-4"] "Каталог"

      -- Панель фильтров
      div_ [class_ "card shadow-sm mb-4", style_ "background: rgba(255,255,255,0.65); border: 1px solid rgba(0,0,0,0.06);"] $ do
        div_ [class_ "card-body"] $ do
          div_ [class_ "row g-3 align-items-end"] $ do

            div_ [class_ "col-12 col-lg-4"] $ do
              label_ [class_ "form-label", for_ "catalog-q"] "Поиск по названию"
              input_ [ class_ "form-control"
                     , id_ "catalog-q"
                     , type_ "search"
                     , placeholder_ "Например: Haskell"
                     ]

            div_ [class_ "col-12 col-md-6 col-lg-2"] $ do
              label_ [class_ "form-label", for_ "catalog-author"] "Автор"
              input_ [ class_ "form-control"
                , id_ "catalog-author"
                , type_ "text"
                , placeholder_ "Все авторы"
                , makeAttribute "list" "catalog-author-list"
                ]
              datalist_ [id_ "catalog-author-list"] mempty

            div_ [class_ "col-12 col-md-6 col-lg-2"] $ do
              label_ [class_ "form-label", for_ "catalog-genre"] "Жанр"
              input_ [ class_ "form-control"
                , id_ "catalog-genre"
                , type_ "text"
                , placeholder_ "Все жанры"
                , makeAttribute "list" "catalog-genre-list"
                ]
              datalist_ [id_ "catalog-genre-list"] mempty

            div_ [class_ "col-6 col-md-3 col-lg-2"] $ do
              label_ [class_ "form-label", for_ "catalog-year-from"] "Год от"
              input_ [class_ "form-control", id_ "catalog-year-from", type_ "number", placeholder_ "—"]

            div_ [class_ "col-6 col-md-3 col-lg-2"] $ do
              label_ [class_ "form-label", for_ "catalog-year-to"] "Год до"
              input_ [class_ "form-control", id_ "catalog-year-to", type_ "number", placeholder_ "—"]

            div_ [class_ "col-12 col-md-6 col-lg-2"] $ do
              button_ [class_ "btn btn-outline-secondary w-100", id_ "catalog-reset", type_ "button"] "Сбросить"

      -- Твой контейнер (его ты уже заполняешь)
      div_ [id_ "book-list", class_ "row g-4"] mempty


    section_ [id_ "about", class_ "app-panel"] $ do
      h2_ [class_ "h2 mb-4"] "О нас"
      p_ "Placeholder: сюда потом добавим нормальный текст."

    section_ [id_ "contact", class_ "app-panel"] $ do
      h2_ [class_ "h2 mb-4"] "Контакты"
      table_ [class_ "table table-striped table-bordered align-middle"] $ do
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
  footer_ [class_ "mt-auto py-4"] $
    div_ [class_ "container text-center text-muted small"] $ do
      p_ "© 2025 Learnbery."


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