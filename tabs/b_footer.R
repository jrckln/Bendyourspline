footer <- tags$footer(style = "
              position: fixed;
              bottom:0;
              z-index: 3;
              width:100%;
              height: 90px;
                      ",
  HTML('
                <style>
                    footer .main-footer{background: #E8E8E8; height: 80px;}
                    footer ul{padding-left: 0;list-style: none;}
                    .footer-copyright {background: #d9230f;padding: 5px 0;}
                    #footercontainer {float: left; width: 100%;}
                    .widget{padding: 10px;	margin-bottom: 20px;}
                    .logo{float: left; width: 25%; padding-bottom: 20px;padding-top: 10px;padding-left: 20px;padding-right: 20px;}
                    #stratos {max-height: 60px; width: 90%;} 
                    #fwf {max-height: 60px; width: 90%;} 
                    #dfg {max-height: 100px; width: 90%;} 
                </style>

                <footer id="footer" class="footer-1">
                    <div class="main-footer widgets-dark typo-light">
                        <div class="container" id="footercontainer">
                            <div class="row" style="width: 100%;">
                                <div class="col-md-2">
                                    <div class="widget no-box">
                                        <h5>  <span></span></h5>
                                        <p> Developed at the Institute of Clinical Biometrics</p>
                                    </div>
                                </div>

                                <div class="col-md-2">
                                    <div class="widget no-box">
                                        <h5> <span></span></h5>
                                        <p> Version  1.1 <br> This work is licensed under a  <a href=https://creativecommons.org/licenses/by/4.0/>CC BY 4.0 license</a></p>
                                    </div>
                                </div>

                                <div class="col-md-2">
                                    <div class="widget no-box">
                                        <h5>Contact Us<span></span></h5>
                                        <p><a href="mailto:biometrie@meduniwien.ac.at?subject=GINGER query">biometrie@meduniwien.ac.at</a></p>
                                        </ul>
                                    </div>
                                </div>
                                <div class="col-md-6">
                                    <div class="logo">
                                        <img src="Meduni-wien.svg" id ="stratos">
                                    </div>
                                    <div class="logo">
                                        <img src="stratos_transparent.gif" id ="stratos">
                                    </div>
                                     <div class="logo">
                                         <img src="fwf-logo-color-transparent-var2.gif" id="fwf">
                                    </div>
                                    <div class="logo">
                                         <img src="DFG-logo-blau.svg" width="100%" id="df">
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
  
                    <div class="footer-copyright">
                    </div>
                </footer>

                 ')
)#end tags_footer