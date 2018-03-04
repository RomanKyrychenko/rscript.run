jsCode <- "shinyjs.pageCol = function(){ 
   $('.container-fluid').css('padding-top', '0%');
   $('.container-fluid').css('padding-bottom', '0%');
   $('.container-fluid').css('padding-left', '0%');
   $('.container-fluid').css('padding-right', '0%');
}"

categories <- list(
  "Населення та міграція" = c("Чисельність населення", "Природний рух населення","Кількість адміністративно-територіальних одиниць","Міграційний рух населення"),
  "Ринок праці" = c("Зайнятість та безробіття", "Оплата праці та соціально-трудові відносини"),
  "Освіта" = c("Дошкільні навчальні заклади","Загальноосвітні навчальні заклади","Професійно-технічні навчальні заклади","Вищі навчальні заклади"),
  "Охорона здоров'я" = c("Заклади охорони здоров’я","Медичні кадри","Захворюваність населення"),
  "Доходи та умови життя"=c("Характеристика домогосподарств","Структура сукупних витрат домогосподарств","Структура сукупних ресурсів домогосподарств","Розподіл населення за рівнем середньодушових загальних доходів","Диференціація життєвого рівня населення","Споживання продуктів харчування в домогосподарствах","Наявність в домогосподарствах окремих товарів тривалого користування"),
  "Соціальний захист"=c("Середній розмір місячної пенсії та кількість пенсіонерів","Чисельність дітей, усиновлених протягом року"),
  "Населені пункти та житло"=c("Житловий фонд України"),
  "Правосуддя та злочинність"=c("Правопорушення"),
  "Культура"=c("Культура","Школи естетичного виховання","Виставкова діяльність")
)

css_style <- '
    button, input, select, textarea {
font-family: inherit;
width: 100%;
font-size: inherit;
line-height: inherit;
}
.skin-blue .main-header .navbar {
background-color: rgb(255, 255, 255);
}
.shiny-input-container:not(.shiny-input-container-inline) {
width: 100%;
max-width: 100%;
}
.skin-blue .main-header .logo {
background-color: #fd0000;
color: #fff;
border-bottom: 0 solid transparent;
}
.navbar-default {
background-color: #f5f5f5;
border-color: #a73131;
}
@media (min-width: 992px)
.container {
width: 100%;
}
@media (min-width: 768px)
.container {
width: 100%;
}
.container {
width: 100%;
margin-right: auto;
margin-left: auto;
}
.skin-blue .main-header .navbar .sidebar-toggle {
color: #f00;
}
.skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
color: #fff;
background: #1e282c;
border-left-color: #ff0000;
}
.shiny-html-output shiny-bound-output {
padding-right: 0%;
padding-left: 0%;
padding-top: 0%;
}
.container-fluid {
padding-right: 30%;
padding-left: 30%;
padding-top: 30%;
padding-bottom: 40%;
margin-right: auto;
margin-left: auto;
}
body {
background-image: url(https://raw.githubusercontent.com/RomanKyrychenko/rscript.run/master/crop3.gif);
}
.well {
min-height: 20px;
padding: 19px;
margin-bottom: 20px;
background-color: rgba(255, 255, 255, 0);
border: 1px solid rgba(191, 29, 29, 0);
/* border-radius: 4px; */
-webkit-box-shadow: inset 0 1px 1px rgba(0,0,0,.05);
box-shadow: inset 0 1px 1px rgba(0,0,0,.05);
}
'