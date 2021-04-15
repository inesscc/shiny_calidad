app <- ShinyDriver$new("../../")
app$snapshotInit("probar_testing")

app$setInputs(base_ine = "click")
app$setInputs(varINTERES = "GASTOT_HD")
app$setInputs(varCRUCE = "ZONA")
app$setInputs(varCRUCE = c("ZONA", "SEXO"))
app$setInputs(actionTAB = "click")
app$snapshot()
