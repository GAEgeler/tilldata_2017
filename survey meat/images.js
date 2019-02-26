document.write("
<!DOCTYPE html>
<html>
<head>
<style>
* {
  box-sizing: border-box;
}

.column {
  float: left;
  width: 25%;
  padding: 2px;
}

/* Clearfix (clear floats) */
.row::after {
  content: "";
  clear: both;
  display: table;
}
</style>
</head>
<body>

<h2>Umfrage zum Fleischkonsum</h2>
<p>Um eine bessere Schätzung abzugeben, sehen Sie ein paar Fleischprodukte mit Grammangaben.</p>

<!-- seems to work however is far from nice -->

<div class="row">
  <div class="column">
  <div style="text-align:center">
    <img src="img/buendnerfleisch_171121_60g.jpg" alt="Bündnerfleisch" style="width:100%">
	<class='small'><strong>Bündnerfleisch 60g </strong></p></div>
  </div>
  <div class="column">
  <div style="text-align:center">
    <img id = "Schweinsplätzli 238g" src="img/plaetzli_171121_238g.jpg" alt="Plätzli" style="width:100%">
	<class='small'><strong>Schweinsplätzli 238g</strong></p></div>
  </div>
  <div class="column">
  <div style="text-align:center">
    <img src="img/pouletschenkel_171121_400g.jpg" alt="Pouletschenkel" style="width:100%">
	<class='small'><strong>Pouletschenkel 400g</strong></p></div>
  </div>
   <div class="column">
   <div style="text-align:center">
    <img src="img/tbone171121_400g.jpg" alt="T-Bone" style="width:100%">
	<class='small'><strong>T-Bone Steak ca. 400g</strong></p></div>
  </div>
</div>

</body>
</html>
");