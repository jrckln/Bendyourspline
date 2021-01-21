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
                    #footercontainer {margin-left: 5%; margin-right: 5%;}
                    .widget{padding: 10px;	margin-bottom: 20px;}

                </style>

                <footer id="footer" class="footer-1">
                    <div class="main-footer widgets-dark typo-light">
                        <div class="container" id="footercontainer">
                            <div class="row">
  
                                <div class="col-xs-12 col-sm-6 col-md-3">
                                    <div class="widget no-box">
                                        <h5>  <span></span></h5>
                                        <p> Developed at the Institute of Clinical Biometrics</p>
                                    </div>
                                </div>

                                <div class="col-xs-12 col-sm-6 col-md-3">
                                    <div class="widget no-box">
                                        <h5> <span></span></h5>
                                        <p> Version  1.1 <br> This work is licensed under a  <a href=https://creativecommons.org/licenses/by/4.0/>CC BY 4.0 license</a></p>
                                    </div>
                                </div>

                                <div class="col-xs-12 col-sm-6 col-md-3">
                                    <div class="widget no-box">
                                        <h5>Contact Us<span></span></h5>
                                        <p><a href="mailto:biometrie@meduniwien.ac.at?subject=GINGER query">biometrie@meduniwien.ac.at</a></p>
                                        </ul>
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