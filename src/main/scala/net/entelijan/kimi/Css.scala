package net.entelijan.kimi

object Css {

  def all: String =
    """.DV_Browser {  display: block;}
      |.DV_MobilePortrait {  display: none;}
      |.DV_MobileLandscape {  display: none;}
      |
      |@font-face {
      |  font-family: 'TradeGothic';
      |  src: url("css/TradeGothicLTStd Cn18.eot?") format("eot"), url("css/TradeGothicLTStd Cn18.woff") format("woff"), url("css/TradeGothicLTStd Cn18.ttf") format("truetype"); }
      |@font-face {
      |  font-family: 'TradeGothic';
      |  src: url("css/TradeGothicLTStd BdCn20.eot?") format("eot"), url("css/TradeGothicLTStd BdCn20.woff") format("woff"), url("css/TradeGothicLTStd BdCn20.ttf") format("truetype");
      |  font-weight: bold; }
      |body {
      |  font-size: 20px;
      |  letter-spacing: 0.02em;
      |  font-family: TradeGothic;
      |  margin: 0px;
      |  margin-top: 50px;
      |  background-color: #ffffff;
      |  color: #000000;
      |  line-height: 0.8em; }
      |
      |#page {
      |  max-width: 1200px;
      |  margin-left: auto;
      |  margin-right: auto; }
      |
      |#content {
      |  z-index: -10;
      |  border-left: 1px solid #d9d9d9;
      |  position: absolute;
      |  top: 100px;
      |  left: 200px;
      |  padding-left: 10px;
      |  padding-top: 65px;
      |  padding-right: 50px;
      |  padding-bottom: 65px; }
      |
      |#contentPrj {
      |  top: 170px;
      |  width: 90%;
      |  height: 40%;
      |  position: absolute;
      |  z-index: 20;
      |  max-width: 1140px; }
      |
      |a:LINK {
      |  text-decoration: none;
      |  color: #000000; }
      |
      |a:VISITED {
      |  text-decoration: none;
      |  color: #000000; }
      |
      |a:ACTIVE {
      |  text-decoration: none;
      |  color: #BBBBBB; }
      |
      |a:HOVER {
      |  text-decoration: none;
      |  color: #BBBBBB; }
      |
      |.ainv:LINK {
      |  text-decoration: none;
      |  color: #BBBBBB; }
      |
      |.ainv:VISITED {
      |  text-decoration: none;
      |  color: #BBBBBB; }
      |
      |.ainv:ACTIVE {
      |  text-decoration: none;
      |  color: #000000; }
      |
      |.ainv:HOVER {
      |  text-decoration: none;
      |  color: #000000; }
      |
      |.nolink, .menuItem, .menuItemF, .menuItemM, .menuItemL, .menuLangItemF, .menuLangItemL {
      |  color: #BBBBBB; }
      |
      |#infoMenu {
      |  margin-bottom: 0px;
      |  border-top: 1px solid #d9d9d9;
      |  margin-top: 0px;
      |  padding-top: 10px;
      |  min-width: 866px; }
      |
      |.infoMenuItemText {
      |  font-weight: bold;
      |  letter-spacing: 0.06em; }
      |
      |.infoMenuItemTextPrefix {
      |  padding-right: 5px; }
      |
      |.menuItemFill {
      |  display: inline;
      |  margin-left: 30px; }
      |
      |.menuItem, .menuItemF, .menuItemM, .menuItemL {
      |  display: inline;
      |  padding-left: 10px;
      |  padding-right: 10px;
      |  margin-top: 0px;
      |  border-top: 0px;
      |  padding-top: 8.7px; }
      |
      |.menuItemM {
      |  border-left: 1px solid #d9d9d9; }
      |
      |.menuItemL {
      |  border-left: 1px solid #d9d9d9;
      |  text-indent: 10px; }
      |
      |.headline {
      |  height: 27px;
      |  min-width: 730px; }
      |
      |.headline1 {
      |  font-size: 35px;
      |  font-weight: bold;
      |  letter-spacing: 0.06em;
      |  color: #830D00;
      |  padding-left: 60px;
      |  padding-right: 10px;
      |  display: inline; }
      |
      |.headline2 {
      |  font-weight: bold;
      |  padding-right: 3px;
      |  display: inline;
      |  letter-spacing: 0.06em; }
      |
      |.headline3 {
      |  display: inline; }
      |
      |.langMenu {
      |  float: right;
      |  padding-right: 30px;
      |  padding-top: 3px; }
      |
      |.menuLangItemF {
      |  font-size: 20px;
      |  font-weight: bold;
      |  letter-spacing: 0.06em;
      |  padding-left: 10px;
      |  padding-right: 10px;
      |  padding-top: 2px;
      |  padding-bottom: 4.5px;
      |  border-right: 1px solid #d9d9d9; }
      |
      |.menuLangItemL {
      |  font-size: 20px;
      |  letter-spacing: 0.06em;
      |  font-weight: bold;
      |  padding-left: 10px;
      |  padding-right: 10px; }
      |
      |#startAphaBlock {
      |  font-size: 40px;
      |  font-weight: bold;
      |  margin-left: auto;
      |  margin-right: auto;
      |  margin-top: 160px;
      |  margin-bottom: 100px;
      |  width: 300px; }
      |
      |#startAlphaBlockBack {
      |  width: 300px;
      |  height: 300px;
      |  margin-left: auto;
      |  margin-right: auto;
      |  margin-top: -10px;
      |  background-image: url("../layout/startbg_50.png");
      |  box-shadow: 6px 6px 25px rgba(0, 0, 0, 0.3); }
      |
      |.startImage {
      |  position: absolute;
      |  display: none;
      |  z-index: -10; }
      |
      |.startChar {
      |  width: 60px;
      |  height: 32px;
      |  padding-top: 27px;
      |  cursor: default;
      |  z-index: auto;
      |  float: left;
      |  text-align: center; }
      |
      |.startImageText, .startImageTextR, .startImageTextL {
      |  height: 15px;
      |  font-size: 12px;
      |  margin-bottom: 3px;
      |  margin-top: 3px; }
      |
      |.startImageTextR {
      |  text-align: right; }
      |
      |.startImageTextL {
      |  text-align: left; }
      |
      |.startImageImage {
      |  box-shadow: 6px 6px 25px rgba(0, 0, 0, 0.3);
      |  background-repeat: no-repeat; }
      |
      |.alphText {
      |  padding-top: 3px; }
      |
      |.alphFill {
      |  padding-top: 20px; }
      |
      |.bioName {
      |  padding-left: 5px;
      |  font-weight: bold;
      |  font-size: 20px;
      |  color: #830D00;
      |  font-weight: bold;
      |  letter-spacing: 0.06em; }
      |
      |.bioTitle {
      |  font-weight: bold;
      |  font-size: 20px;
      |  letter-spacing: 0.06em; }
      |
      |.bioText {
      |  margin-top: 10px;
      |  margin-bottom: 10px;
      |  font-size: 15px;
      |  width: 300px; }
      |
      |.bioAdr {
      |  margin-top: 30px; }
      |
      |.bioCredit {
      |  margin-top: 60px;
      |  font-size: 12px; }
      |
      |.prjImg {
      |  float: left;
      |  margin-right: 15px;
      |  box-shadow: 6px 6px 25px rgba(0, 0, 0, 0.3); }
      |
      |.prjTitle {
      |  color: #BBBBBB;
      |  font-weight: bold;
      |  letter-spacing: 0.06em;
      |  padding-top: 5px; }
      |
      |.prjSubtitle {
      |  padding-top: 6px;
      |  font-size: 15px; }
      |
      |.prjText {
      |  font-size: 15px; }
      |
      |.prjCatHeading {
      |  margin-top: 20px;
      |  margin-bottom: 2px;
      |  color: #BBBBBB;
      |  font-weight: bold;
      |  letter-spacing: 0.06em; }
      |
      |.prjCatLink {
      |  margin-left: 40px;
      |  font-size: 15px; }
      |""".stripMargin

  def mobile: String = {
    """#mobileContent {
      |  z-index: -10;
      |  position: absolute;
      |  top: 120px;
      |  left: 10px;
      |  padding-left: 10px;
      |  padding-top: 65px;
      |  padding-right: 50px;
      |  padding-bottom: 65px; }
      |
      |#mobileContentPrj {
      |  margin-top: 20px;
      |  padding-left: 20px;
      |  top: 170px;
      |  width: 90%;
      |  height: 40%;
      |  position: absolute;
      |  z-index: 20; }
      |
      |.menuItemFill {
      |  margin-left: 10px; }
      |
      |.headlineB {
      |  padding-top: 5px;
      |  padding-left: 20px;
      |  height: 43px;
      |  margin-top: -17px; }
      |
      |.headline1 {
      |  padding-left: 20px }
      |
      |#startAlphaBlockBack {
      |    margin-top: -100px;
      |    margin-bottom: 100px; }
      |
      |#infoMenu {
      |  min-width: 400px; }
      |
      |.headline {
      |  min-width: 400px;
      |  height: 42px;
      |}
      |
      |""".stripMargin
  }

  def mobilePortrait: String = {
    s"""$mobile
       |.DV_Browser {  display: none;}
       |.DV_MobilePortrait {
       |  zoom: 85%;
       |  display: block;}
       |.DV_MobileLandscape {  display: none;}
       |
       |body {
       |  line-height: 0.7em;
       |}
       |
       |#mobileContentPrj {
       |  max-width: 400px; }
       |
       |""".stripMargin
  }

  def mobileLandscape: String = {
    s"""$mobile
       |.DV_Browser {  display: none;}
       |.DV_MobilePortrait {  display: none;}
       |.DV_MobileLandscape {  display: block;}
       |
       |body {
       |  line-height: 0.8em;
       |}
       |
       |#mobileContentPrj {
       |  margin-top: 0px;
       |  max-width: 620px; }
       |
       |""".stripMargin
  }
}