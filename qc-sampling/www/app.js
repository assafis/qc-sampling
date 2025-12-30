$(document).ready(function(){
	
});

function editName(fobj){
  var newval = $(fobj).parent().find('.fname').html();
  newval = newval.substr(0,newval.length - 4);
  $('.fname').show();
  $('.fa-edit, .delicon').show();
  $('.newname, .delfile').hide();
  $(fobj).parent().find('.fname').hide();
  $(fobj).parent().find('.fa-edit').hide();
  $(fobj).parent().find('.newname input').val(newval);
  $(fobj).parent().find('.newname').show();
}

function cancelEdit(){
  $('.newname, .delfile').hide();
  $('.fname').show();
  $('.fa-edit, .delicon').show();
}


function renameFile(fobj){
  var newval = $(fobj).parent().find('input').val();
  var oldval = $(fobj).parent().parent().find('.fname').html();
  newval = newval + '.csv';
//  if(newval.length < 4 || newval.trim()==oldval.trim()) {return;}  
//  console.log(oldval + ' > ' + newval);
  Shiny.setInputValue("newFileName", newval, {priority: "event"});
  Shiny.setInputValue("oldFileName", oldval, {priority: "event"});
  //$(fobj).parent().find('.fa-edit').hide();
}

function deleteFile(fobj){
  $('.fname').show();
  $('.fa-edit, .delicon').show();
  $('.newname, .delfile').hide();
  $(fobj).parent().find('.delfile').show();
  $(fobj).parent().find('.delicon').hide();
}

function approveDelete(fobj){
  var fpath = $(fobj).parent().parent().find('.fpath').html().trim();
  console.log(fpath);
  Shiny.setInputValue("stageDelete", fpath, {priority: "event"});
}


function downloadCSV(fobj){
  var fpath = $(fobj).parent().find('.fpath').html();
  Shiny.setInputValue("stageDownload", fpath, {priority: "event"});
  console.log(fpath);

 // $('#stageDownload').val(fname)
  Shiny.setInputValue("stageDownload", fpath, {priority: "event"});

}


function newProj(){
  var fpath = $('newproj').val();
  //Shiny.setInputValue("stageDelete", fpath, {priority: "event"});
}
