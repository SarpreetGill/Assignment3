// get variables from 1. get_colnames_from_R.py

function get_variables(){
  var var_names = ['SEQN', 'URXUMA', 'URXUMS', 'URXUCR.x', 'URXCRS', 'URDACT', 'LBXWBCSI', 'LBXLYPCT', 'LBXMOPCT', 'LBXNEPCT', 'LBXEOPCT', 'LBXBAPCT', 'LBDLYMNO', 'LBDMONO', 'LBDNENO', 'LBDEONO', 'LBDBANO', 'LBXRBCSI', 'LBXHGB', 'LBXHCT', 'LBXMCVSI', 'LBXMCHSI', 'LBXMC', 'LBXRDW', 'LBXPLTSI', 'LBXMPSI', 'PHQ020', 'PHQ030', 'PHQ040', 'PHQ050', 'PHQ060', 'PHAFSTHR.x', 'PHAFSTMN.x', 'PHDSESN', 'LBDHDD', 'LBDHDDSI', 'LBXHA', 'LBXHBS', 'LBXHBC', 'LBDHBG', 'LBDHD', 'LBDHEG', 'LBDHEM', 'LBXTC', 'LBDTCSI', 'LBXTTG', 'URXVOL1'];
  var var_descs = new Array(var_names.length);
  var rows = document.querySelector('#GridView1 tbody').children;
  for (var n=0; n < rows.length; n++) {
	var row = rows[n];
    var cols = row.children;
    var var_name = cols[0].innerText;
    var var_desc = cols[1].innerText;
	if (var_names.indexOf(var_name) != -1) {
		var_descs[var_names.indexOf(var_name)] = var_desc;
	}
  }
  for (var n=0; n < var_descs.length; n++) {
    if (var_descs[n] === undefined) {
	  var_descs[n] = "";
	}
  }
  return var_descs;
}