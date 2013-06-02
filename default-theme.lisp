
;;; styles.lisp --- Built-in styles for Nestor

;;; Copyright (C) 2012 Joao Tavora

;;; Author: Joao Tavora <joaotavora@gmail.com>
;;; Keywords:

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

;;;; CL-WHO Layouts
;;;;
(in-package :nestor-default-theme)

(deftemplate master ()
  "Render CONTENT in the default layout."
  (:html
   (:head
    (loop for script in '()
          do (htm (:script :src script :type "text/javascript")))
    (loop for script in '("/css/master.css")
          do (htm (:link :href script :media "screen" :rel "stylesheet" :type "text/css"))))
   (:body
    (:div :id "container"
          (:div :id "header" (htm (header)))
          (:div :id "content" (str (to-html *page*)))
          (:div :id "footer" (htm (footer)))))))

(deftemplate page (content)
  "Render CONTENT in the default page template"
  (:nav :class "breadcrumb"
        (str "foo > bar > breadcrumbing..."))
  (:div :id "content"
        (:article :role "main"
                  (str content)
                  (htm (related-summaries *page*)))))


(deftemplate related-summaries (page)
  (:section :class "pages"
            (:ol
             (loop for article in (pages-about page)
                   do (str (page-summary article)))))
  (loop for category in (categories page)
        do (htm (:section :class "articles"
                          (:header
                           (:h1 (articles-heading category)))
                          (:ol
                           (loop for article in (pages-about category)
                                 do (htm (page-summary article))))))))

(deftemplate page-summary (page)
  (:li
     (:article
      (:header
       (:h1
        (:a :href (file page)
            (read-more page))))
      (if (or (date page)
              (categories page))
          (htm (page-meta page))))))

(deftemplate page-meta (page)
  (:footer
   (:p :class "meta"
       "Published "
       (if (date page)
           (htm (str "on ")
                (:time :datetime (str (date page))
                       :pubdate "true"
                       (format-date (date page)))))
       (if (categories page)
           (htm (str "in ")
                (loop for (c1 more) on (categories page) by #'cdr
                      do (htm (:a :href (file c1)
                                  (str (title c1)))
                              (when more
                                (str ", ")))))))))

(deftemplate header ()
  (:header :role "banner"
           (:hgroup
            (:h1 (str (heading)))
            (:h1 (str (subtitle))))))


(deftemplate footer ()
  (:footer :class "branding"
           (:p "Powered by Nestor, a "
               (:a :href "http://github.com/capitaomorte/nestor"
                   "Common-Lisp CMS")
               ".")))

(deftemplate sidebar ()
  (:div :id "sidebar"
        (str (sidebar-categories))
        (str (sidebar-feed))))

(deftemplate sidebar-categories ()
  (when (menu-items)
    (cl-who:htm (:nav :class "categories"
                      (:h1 "Articles by category")
                      (nestor-view:display-menu (menu-items) :class "menu")))))


(deftemplate sidebar-feed (&optional (title "Atom feed"))
  (:div :class "feed"
        (:a :href "/articles.xml"
            :title title
            (str title))))

;; (defstyle master
;;   ("*"
;;     :padding 0
;;     :margin 0)
;;   ("body"
;;    :color "#262631"
;;    :line-height "1.75em"
;;    :font "0.924em Georgia, serif")
;;   ("header[role=banner] h1,header[role=banner] h2"
;;    :font-weight "normal"
;;    :line-height "1.2em"
;;    :margin 0)
;;   ("header[role=banner] h1"
;;    :text-shadow "0 2px 3px #dddddd"
;;    :font-size "327%")
;;   ("header[role=banner] h2"
;;    :font-size "1em"
;;    :color "#87877d")
;;   ("h1, h2, h3, h4, h5, h6"
;;    :font-family "Georgia, serif")
;;   ("h1"
;;    :font-weight "normal"
;;    :font-size "218.0%"
;;    :line-height "1.6055em"
;;    :margin-bottom "0.3211em"
;;    :margin-top "0.48165em")
;;   ("h2"
;;    :font-weight "normal"
;;    :font-size "164%"
;;    :line-height "1.06707em"
;;    :margin-bottom "0.53354em"
;;    :margin-top "1.60061em")
;;   ("h3"
;;    :font-weight "normal"
;;    :font-size "145%"
;;    :line-height "1.2069em"
;;    :margin-bottom "0.60345em"
;;    :margin-top "1.81034em")
;;   ("h4"
;;    :font-size "118%"
;;    :line-height "1.48305em"
;;    :margin-bottom "0em"
;;    :margin-top "2.9661em")
;;   ("ol,p,pre,ul"
;;    :margin-bottom "1.75em"
;;    :margin 0)
;;   ("li"
;;    :font-size "100%"
;;    :line-height "1.75em"
;;    :margin-bottom "0em"
;;    :margin-top "0em")
;;   ("blockquote"
;;    :color "#7b7b86"
;;    :font-style "italic"
;;    :padding "0 1.75em"
;;    :margin "1.75em 0")
;;   ("pre"
;;    :overflow "auto"
;;    :padding "0.875em 1em")
;;   ("img"
;;    :border "none")
;;   ("nav.breadcrumb"
;;    :font-size "0.909em"
;;    :padding "0.5em 0"
;;    :color "#87877d"
;;    :margin-top "1.75em")
;;   ("article, aside, footer, header, nav, section"
;;    :display "block")
;;   ("div#container"
;;    :padding "1em 1em 0 1em"
;;    :margin "0 auto"
;;    :width "54em")
;;   ("div#container div#content"
;;    :padding "1px 0"
;;    :float "left"
;;    :width "37em"
;;    :position "relative")
;;   ("div#container div#sidebar"
;;    :padding "0 1em"
;;    :margin-left "40em"
;;    :width "12em")
;;   ("div#container footer.branding"
;;    :font-size "88%"
;;    :line-height "1.98864em"
;;    :margin-bottom "1.98864em"
;;    :margin-top "1.98864em"
;;    :color "#87877d"
;;    :clear "both")
;;   ("div#container footer.branding p"
;;    :padding "1em 0"
;;    :margin 0
;;    :width "37em")
;;   ("a"
;;    :transition "color 0.25s 0 ease"
;;    :-webkit-transition "color 0.25s 0 ease"
;;    :-o-transition "color 0.25s 0 ease"
;;    :-moz-transition "color 0.25s 0 ease"
;;    :color "#006dd1"
;;    :text-decoration "none"
;;    :border-bottom "1px dotted #006dd1")
;;   ("a:visited"
;;    :border-bottom-color "#0060b7"
;;    :color "#0060b7")
;;   ("a:hover"
;;    :border-bottom-color "#1f94ff"
;;    :color "#1f94ff")
;;   ("a:active"
;;    :border-bottom-color "#00386b"
;;    :color "#00386b")
;;   ("nav.breadcrumb ul"
;;    :margin 0)
;;   ("nav.breadcrumb li"
;;    :list-style "none"
;;    :display "inline")
;;   ("nav.breadcrumb li::after"
;;    :content "" > "")
;;   ("nav.breadcrumb li:last-child::after"
;;    :content """")
;;   ("nav.breadcrumb a,nav.categories a,div.feed a,article p.meta a"
;;    :color "#70abe1")
;;   ("nav.breadcrumb:hover a,nav.categories:hover a,div.feed:hover a,article p.meta:hover a"
;;    :color "#006dd1")
;;   ("nav.breadcrumb a:hover,nav.categories a:hover,div.feed a:hover,article p.meta a:hover"
;;    :color "#1f94ff")
;;   ("nav.categories a,div.feed a,article p.meta a"
;;    :border-bottom-color "white")
;;   ("article p.meta a"
;;    :transition "border-bottom-color 0.5s 0 ease"
;;    :-webkit-transition "border-bottom-color 0.5s 0 ease"
;;    :-o-transition "border-bottom-color 0.5s 0 ease"
;;    :-moz-transition "border-bottom-color 0.5s 0 ease")
;;   ("article p.meta a:hover"
;;    :border-bottom-color "#1f94ff")
;;   ("article h1 a"
;;    :border-bottom "none")
;;   ("body"
;;    :background "white")
;;   ("article img"
;;    :margin-bottom "1.75em"
;;    :max-width "100%")
;;   ("article code,article pre"
;;    :background-color "#eaf4f7")
;;   ("article code"
;;    :padding "1px 3px")
;;   ("article pre"
;;    :background-color "#eaf4f7"
;;    :border-left "1px dashed #71adce")
;;   ("article pre code"
;;    :padding 0)
;;   ("article footer"
;;    :border-top "1px dashed #71adce")
;;   ("article footer p.meta"
;;    :color "#87877d"
;;    :font-style "italic"
;;    :font-size "90.9%"
;;    :line-height "1.92519em"
;;    :margin-bottom "3.65787em"
;;    :margin-top "0.19252em")
;;   ("article[role=\"main\"] h1, article[role=\"main\"] h2"
;;    :text-shadow "0 2px 3px #dddddd")
;;   ("article[role=\"main\"] div#disqus_thread img"
;;    :max-width "none")
;;   ("article[role=\"main\"] div#disqus_thread ul#dsq-comments"
;;    :margin-left 0)
;;   ("section.pages > ol,section.articles > ol"
;;    :margin-left 0)
;;   ("section.pages > ol li,section.articles > ol li"
;;    :list-style "none"
;;    :position "relative")
;;   ("section.pages > ol article ol li,section.articles > ol article ol li"
;;    :list-style "decimal")
;;   ("section.pages > ol article ul li,section.articles > ol article ul li"
;;    :list-style "disc")
;;   ("section.pages header[role=main] h1,section.articles header[role=main] h1"
;;    :font-size "218.0%"
;;    :line-height "1.6055em"
;;    :margin-bottom "0.40138em"
;;    :margin-top "1.20413em")
;;   ("section.pages header h1,section.articles header h1"
;;    :font-size "164%"
;;    :line-height "1.06707em"
;;    :margin-bottom "0.53354em"
;;    :margin-top "1.60061em")
;;   ("section.pages article h1,section.articles article h1"
;;    :text-shadow "none")
;;   ("section.pages article p.read_more,section.articles article p.read_more"
;;    :margin-top "-1.75em"
;;    :font-size "100%"
;;    :line-height "1.75em"
;;    :margin-bottom "0em"
;;    :margin-top "0em")
;;   ("section.pages article footer,section.articles article footer"
;;    :border-top "none")
;;   ("nav.categories h1"
;;    :font-size "100%"
;;    :line-height "1.75em"
;;    :margin-bottom "0em"
;;    :margin-top "3.5em")
;;   ("nav.categories ul.menu"
;;    :list-style "none")
;;   ("nav.categories ul.menu ul"
;;    :margin "0 0 0 1.25em"
;;    :list-style "disc")
;;   ("nav.categories ul.menu > ul"
;;    :font-size "90.9%"
;;    :line-height "1.92519em"
;;    :margin-bottom "0em"
;;    :margin-top "0em")
;;   ("div.feed"
;;    :margin "1.75em 0"))
