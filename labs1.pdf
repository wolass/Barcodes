<!DOCTYPE html>
<html>
<head>

  <meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
  <script type="application/shiny-singletons"></script>
  <script type="application/html-dependencies">json2[2014.02.04];jquery[3.4.1];shiny[1.4.0];font-awesome[5.3.1];bootstrap[3.4.1];AdminLTE[2.0.6];shinydashboard[0.7.1]</script>
<script src="shared/json2-min.js"></script>
<script src="shared/jquery.min.js"></script>
<link href="shared/shiny.css" rel="stylesheet" />
<script src="shared/shiny.min.js"></script>
<link href="font-awesome-5.3.1/css/all.min.css" rel="stylesheet" />
<link href="font-awesome-5.3.1/css/v4-shims.min.css" rel="stylesheet" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<link href="shared/bootstrap/css/bootstrap.min.css" rel="stylesheet" />
<script src="shared/bootstrap/js/bootstrap.min.js"></script>
<script src="shared/bootstrap/shim/html5shiv.min.js"></script>
<script src="shared/bootstrap/shim/respond.min.js"></script>
<link href="AdminLTE-2.0.6/AdminLTE.min.css" rel="stylesheet" />
<link href="AdminLTE-2.0.6/_all-skins.min.css" rel="stylesheet" />
<script src="AdminLTE-2.0.6/app.min.js"></script>
<link href="shinydashboard-0.7.1/shinydashboard.css" rel="stylesheet" />
<script src="shinydashboard-0.7.1/shinydashboard.min.js"></script>  <title>QR code Labels generator from clinical samples</title>

</head>

<body class="skin-blue" style="min-height: 611px;">
  <div class="wrapper">
    <header class="main-header">
      <span class="logo">QR code Labels generator from clinical samples</span>
      <nav class="navbar navbar-static-top" role="navigation">
        <span style="display:none;">
          <i class="fa fa-bars"></i>
        </span>
        <a href="#" class="sidebar-toggle" data-toggle="offcanvas" role="button">
          <span class="sr-only">Toggle navigation</span>
        </a>
        <div class="navbar-custom-menu">
          <ul class="nav navbar-nav"></ul>
        </div>
      </nav>
    </header>
    <aside id="sidebarCollapsed" class="main-sidebar" data-collapsed="false">
      <section id="sidebarItemExpanded" class="sidebar"></section>
    </aside>
    <div class="content-wrapper">
      <section class="content">
        <div class="row">
          <div class="col-sm-6">
            <div class="box">
              <div class="box-body">
                <div class="form-group shiny-input-container">
                  <label class="control-label" for="patient_n">Patient number</label>
                  <input id="patient_n" type="text" class="form-control" value="1"/>
                </div>
              </div>
            </div>
          </div>
        </div>
        <div class="row">
          <div class="col-sm-6">
            <div class="box">
              <div class="box-body">
                <button id="generateB" type="button" class="btn btn-default action-button">Generate labels</button>
              </div>
            </div>
          </div>
          <div class="col-sm-6">
            <div class="box">
              <div class="box-body">
                <a id="downloadLabels" class="btn btn-default shiny-download-link " href="" target="_blank" download>
                  <i class="fa fa-download"></i>
                  Download
                </a>
              </div>
            </div>
          </div>
        </div>
      </section>
    </div>
  </div>
</body>

</html>
