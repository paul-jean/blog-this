(* ::Package:: *)

(* Mathematica Package *)

(* Created by the Wolfram Workbench Jan 23, 2011 *)

BeginPackage["BlogClient`", "JLink`"]
(* Exported symbols added here with SymbolName::usage *) 

BlogExport::usage = "BlogExport[nbfile_String, htmlfile_String, date_String]"
BlogRequest::usage = "BlogRequest[rpcmethod_String, params_List, opts]"
PostNotebook::usage = "PostNotebook[nbfile_String, opts]"

Begin["`Private`"]
(* Implementation of the package *)

$DebugQ = False;

DebugPrint[s___] := Print["["<>DateString[]<>"]"<>" ", s] /; $DebugQ

styled[s_String] := Style[s, 12, FontFamily->"Helvetica"]

DebugPrint["Loadig BlogClient` ...."]

InstallJava[]



Options[BlogExport] = {
	"SiteAddress"->Automatic, 
	"Port"->Automatic, 
	"WordPressPath"->Automatic, 
	"ConversionRules"->{
		"Subsection"->{"<h2>", "</h2>"},
		"Section"->{"<h1>", "</h1>"},
		"Text"->{"<p>", "</p>"}
		},
	"MathOutput"->Automatic
	};

BlogExport[nbfile_String, htmlfile_String, date_String, opts:OptionsPattern[]] := Module[
  {nb, t, t1, stream, siteaddress, port, wppath, conversionrules, mathoutput},
  siteaddress = OptionValue["SiteAddress"];
  port = OptionValue["Port"];
  port = If[MatchQ[port, _Integer], ":"<>IntegerString@port, ""];
  (* include a trailing slash on the WP path, but remove the leading slash *)
  wppath = OptionValue["WordPressPath"] /. Automatic -> "";
  If[ !StringMatchQ[wppath, ""],
  	wppath = If[StringMatchQ[StringTake[wppath, 1], "/"], StringTake[wppath, {2,-1}], wppath];
  	wppath = wppath<>If[StringMatchQ[StringTake[wppath, -1], "/"], "", "/"];
  ];
  conversionrules = OptionValue["ConversionRules"];
  mathoutput = OptionValue["MathOutput"];
  (*DebugPrint["ConversionRules"->conversionrules];*)
  nb = NotebookOpen[nbfile, Visible -> False];
  SetOptions[nb, WindowSize -> {500, Automatic}];
  Export[htmlfile, nb, "HTML", "FullDocument"->False, "ConversionRules"->conversionrules, "MathOutput"->mathoutput];
  t = Import[htmlfile, "Text"];
  (* TODO: Use ConversionRules instead of this StringReplace hack *)
  t1 = StringReplace[
  	t,
  	{
  		"HTMLFiles/" -> "http://"<>siteaddress<>port<>"/"<>wppath<>"wp-content/uploads/"<>date<>"/", 
  		" style='font-size: 16px;'" -> "",
		(* "MathOutput" -> "InputForm": *)
  		(* strip InputFormInline style class *)
  		RegularExpression["<span class=\"InputFormInline\">(.*?)</span>"] :> "$1",
		(* "MathOutput" -> "InputForm": *)
  		(* strip OutputFormInline style class *)
  		RegularExpression["<span class=\"OutputFormInline\">(.*?)</span>"] :> "$1",
		(* "MathOutput" -> "DisplayForm" *)
  		(* strip OutputFormInline style class *)
		RegularExpression["(?s)(<pre class='mycode'>.*?</pre>)"] :> With[{pre="$1"},
			StringReplace[
				StringReplace[pre, RegularExpression["(?s)<span class=\"OutputFormInline\">(.*?)</span>"] :> "$1"], 
				RegularExpression["\\n\\s*?\\n"] :> "\n"
				]
			] 		
  		(*RegularExpression["width=\"\\d+\""] :> ""*)
  		(*RegularExpression["height=\"\\d+\""] :> ""*)
  	}];
  stream = OpenWrite[htmlfile];
  WriteString[stream, t1];
  Close[stream];
  htmlfile
  ]

xmlrpcencode[i_Integer] := 
 XMLElement["value", {}, {XMLElement["i4", {}, {ToString@i}]}]

xmlrpcencode[s_String] := 
 XMLElement["value", {}, {XMLElement["string", {}, {s}]}]

xmlrpcencode[r_Real] := 
 XMLElement["value", {}, {XMLElement["double", {}, {ToString@r}]}]

xmlrpcencode["Base64"[b64_String]] := 
 XMLElement["value", {}, {XMLElement["base64", {}, {b64}]}]

xmlrpcencode[bool : True | False] := 
 XMLElement[
  "value", {}, {XMLElement[
    "boolean", {}, {Replace[bool, {False -> "0", True -> "1"}]}]}]

xmlrpcencode[date : Hold[DateString[__]]] := 
 XMLElement[
  "value", {}, {XMLElement[
    "dateTime.iso8601", {}, {ReleaseHold@date}]}]

xmlrpcencode[struct : {___Rule}] := 
 XMLElement[
  "value", {}, {XMLElement["struct", {}, 
    XMLElement[
       "member", {}, {XMLElement["name", {}, {First@#}], 
        xmlrpcencode[Last@#]}] & /@ struct]}]

xmlrpcencode[array : {___}] := 
 XMLElement[
  "value", {}, {XMLElement[
    "array", {}, {XMLElement["data", {}, xmlrpcencode /@ array]}]}]

xmlrpcencode[methodname_String, params_List] := 
 XMLObject["Document"][{}, 
  XMLElement[
   "methodCall", {}, {XMLElement["methodName", {}, {methodname}], 
    XMLElement["params", {}, 
     XMLElement["param", {}, {xmlrpcencode@#}] & /@ params]}], {}]

xmlrpcparse[
  XMLObject["Document"][_, 
   XMLElement[
    "methodResponse", {}, {XMLElement["params", {}, 
      params : {__XMLElement}]}], {}]] := xmlrpcparse /@ params

xmlrpcparse[
  XMLObject[
    "Document"][{XMLObject["Declaration"]["Version" -> "1.0"]}, 
   XMLElement[
    "methodResponse", {}, {XMLElement["fault", {}, 
      details : {__XMLElement}]}], {}]] := xmlrpcparse /@ details

xmlrpcparse[
  XMLElement[
   "param", {}, {value : XMLElement["value", {}, {__XMLElement}]}]] :=
  xmlrpcparse@value

xmlrpcparse[
  obj : XMLElement[
    "value", {}, {XMLElement[type_String, {}, val_]}]] := Block[{},
  Switch[{type, val},
   {"int" | "i4", {_String}}, ToExpression[First@val],
   {"double", {_String}}, ToExpression@First@val,
   {"string", {_String}}, First@val,
   {"dateTime.iso8601", {_String}}, First@val,
   {"boolean", {_String}}, 
   Replace[First@val, {"1" -> True, "0" -> False}],
   {"base64", {_String}}, ImportString[First@val, "Base64"],
   {"struct", {__XMLElement}}, xmlrpcparse[obj],
   {"array", {_XMLElement}}, xmlrpcparse[obj],
   {_, {}}, val
   ]
  ]

xmlrpcparse[
  XMLElement[
   "value", {}, {XMLElement["struct", {}, 
     members : {___XMLElement}]}]] := 
 Cases[members, 
  XMLElement[
    "member", {}, {XMLElement["name", {}, {name_String}], 
     XMLElement["value", {}, {XMLElement[type_, {}, val_]}]}] :> 
   name -> xmlrpcparse[
     XMLElement["value", {}, {XMLElement[type, {}, val]}]], {1}]

xmlrpcparse[
  XMLElement[
   "value", {}, {XMLElement[
     "array", {}, {XMLElement["data", {}, 
       values : {___XMLElement}]}]}]] :=
 Cases[values, XMLElement
    ["value", {}, {XMLElement[type_, {}, val_]}] :> 
   xmlrpcparse[XMLElement
     ["value", {}, {XMLElement[type, {}, val]}]], {1}]

Options[BlogRequest] = {"Username" -> Automatic, 
   "Password" -> Automatic, "SiteAddress" -> Automatic, 
   "Port" -> Automatic,"XMLRPCServerPath" -> "/xmlrpc.php"};

BlogRequest[rpcmethod_String, params_List, opts : OptionsPattern[]] :=
  JLink`JavaBlock@Module[{username, password, siteaddress, port, xmlrpcserverpath, 
    client, creds, scope, xmlrpcserveraddress, requestentity, payload,
     returncode, responsexml, response, method, urlport},
   username = OptionValue["Username"];
   password = OptionValue["Password"];
   siteaddress = OptionValue["SiteAddress"];
   port = OptionValue["Port"];
   urlport = Switch[port, 
   		_Integer, ":"<>IntegerString@port, 
   		s_String /; StringMatchQ[s, DigitCharacter..], ":"<>port,
   		_, ""
   ];
   (* TODO: handle empty string for xmlrpc server option *)
   xmlrpcserverpath = OptionValue["XMLRPCServerPath"];
   xmlrpcserverpath = If[
   	StringMatchQ[StringTake[xmlrpcserverpath, 1], "/"], 
   	StringTake[xmlrpcserverpath, {2, -1}], 
   	xmlrpcserverpath
   ];

   client = 
    JLink`JavaNew["org.apache.commons.httpclient.HttpClient"];
   creds = 
    JLink`JavaNew[
     "org.apache.commons.httpclient.UsernamePasswordCredentials", 
     username, password];
   JLink`LoadJavaClass["org.apache.commons.httpclient.auth.AuthScope"];
   (* Why can't I use port 443 to encrypt the request over HTTPS? *)
   (* + See http://blog.wolfram.com/2009/04/30/twittering-with-mathematica/ *)
   scope = 
    JLink`JavaNew["org.apache.commons.httpclient.auth.AuthScope", 
     siteaddress, 8888, AuthScope`ANYUREALM];
   client@getState[]@setCredentials[scope, creds];
   xmlrpcserveraddress = "http://" <> siteaddress <> urlport <> "/" <> xmlrpcserverpath;
   (*DebugPrint[xmlrpcserveraddress];*)
   method = JLink`JavaNew["org.apache.commons.httpclient.methods.PostMethod", xmlrpcserveraddress];
   
   payload = ExportString[xmlrpcencode[rpcmethod, params], "XML"];
   
   requestentity = 
    JLink`JavaNew[
     "org.apache.commons.httpclient.methods.StringRequestEntity", 
     payload, "text/xml", "UTF-8"];
   method@setRequestEntity[requestentity];
   returncode = client@executeMethod[method];
   responsexml = method@getResponseBodyAsString[];
   response = ImportString[responsexml, "XML"];
   method@releaseConnection[];
   
   {
   	"ReturnCode" -> returncode, 
   	"ResponseXML" -> responsexml,
    "Response" -> xmlrpcparse@response
   }
   
   ]

Options[PostNotebook] = Join[
	Options[BlogRequest],
	Options[BlogExport],
	{
		"Title"->Automatic, 
		"Debug"->False, 
		"Categories"->{}
	}
];
(*TODO: "WordPressPath" should be an option for BlogRequest *)

PostNotebook[nbfile_String, opts:OptionsPattern[]] := 
Module[ 
	{localexportdir, htmlfile, newpostid, posttitle, username, password,
    newpostlink, server, port, serverpath, date, debugQ, htmlstring, categories,
    existingcategories, createdcategoryids, existingcategorynames, addcategory,
    postcategories, imagefiles},
	posttitle = OptionValue["Title"] /. Automatic -> FileBaseName[nbfile];
    username = OptionValue["Username"];
	password = OptionValue["Password"];
	server = OptionValue["SiteAddress"];
	port = OptionValue["Port"];
	serverpath = OptionValue["WordPressPath"] /. Automatic -> "";
	debugQ = TrueQ@OptionValue["Debug"];
	categories = OptionValue["Categories"];
    
	If[debugQ, $DebugQ=True];

	(* add categories if not present already *)
	(* category can either be a String or a List *)
	(* if it's a list, then it's assumed that it's a nested category *)
	(* e.g.  
	"rule18" 
	{"rule146", "monoliths"}
	{"2010", "08", "rule146", "monoliths"}
	
	"Categories"->{"rule18", {"2010", "08", "rule146", "monoliths"}}
	
	*)
	(* http://codex.wordpress.org/XML-RPC_wp # wp.getCategories *)
	existingcategories =
	{"categoryName", "categoryId", "parentId"}/.#& /@
	Flatten[ 
		"Response" /. 
		BlogRequest[
			"wp.getCategories", 
			{1, username, password}, 
			FilterRules[{opts}, Options@BlogRequest]
		],
		1
	];
	existingcategorynames = existingcategories[[All,1]];

	(* +++++++++++++++++++++++++++++++++ *)
	(* helper function to add a category *)
	(* returns: category ID just created *)
	addcategory[catname_String, parentid_Integer]:=Module[
		{slug, response, catid},
		(*TODO: deal with potential spaces in category name for slug *)
		slug = catname;
		(*TODO: optionally take a category description? *)
		If[(catid = Cases[existingcategorynames, {catname, id_, parentid}]:>id) === {_Integer}, 
			Return[First@catid]
		];
		(* http://codex.wordpress.org/XML-RPC_wp # wp.newCategory *)
		response = "Response" /.
		BlogRequest["wp.newCategory", 
			{
				1, username, password, 
				{
					"name" -> catname, 
					"slug" -> slug, 
    				"parent_id" -> parentid, 
    				"description" -> ""
    			}
    		}, 
  			FilterRules[{opts}, Options@BlogRequest]
  		];
  		(*TODO: what if the response is $Failed? *)
  		catid = If[
  			MatchQ[Flatten[response, 1], {_Integer}],
  			First[Flatten[response, 1]],
  			$Failed
  		];
  		catid
	];
	(* +++++++++++++++++++++++++++++++++ *)

	createdcategoryids = With[{cat=#},
		Switch[
			cat,
			_String, (* e.g. "rule18" *)
			(* parent ID for a string category is always 0 *)
			addcategory[cat, 0],
			_List, (* e.g. {"2010", "08", "rule146", "monoliths"} *)
			(* assign parent IDs sequentially *)
			(* for each category name in the hierarchy list *)
			(* if it's an existing category, extract the parent ID *)
			(* if it's not, then create the category, and assign the parent ID to the category before it in the list *)
			(* the first category in the list has parent ID = 0 *)
			Catch@Module[{catlist=cat, catid},
				FoldList[If[MatchQ[(catid=addcategory[#2, #1]), _Integer], catid, Throw[$Failed]]&, 0, catlist]
			]
	]]& /@ categories;
	(* select the categories that were successfully added to WordPress *)
	(* two cases for each category: *)
	(* cat_String, e.g. "rule18": use 'cat' as the category *)
	(* cat_List, e.g. {"2010", "08", "rule146", "monoliths"}: 
	   use the last category in the list as the category name (leaf node in category tree) *)
	postcategories = 
	MapThread[
		If[FreeQ[#2, $Failed], Switch[#1, _String, #1, _List, Last@#1], $Failed]&, 
		{categories, createdcategoryids}
	];
	DebugPrint["Categories:"];
	DebugPrint[postcategories];
    localexportdir = FileNameTake[nbfile, {1, -2}];
    (* TODO: have an option "PostDate" to set the date of publication of the post *)
    (*+ i.e. use 'dateCreated' field *)
    date = DateString[{"Year", "/", "Month"}];
    htmlfile = FileNameJoin[{localexportdir, FileBaseName[nbfile] <> ".html"}];
    (*DebugPrint[Options@BlogExport];*)
	DebugPrint["Exporting html ..."];
    BlogExport[nbfile, htmlfile, date, Sequence@@FilterRules[{opts}, Options@BlogExport]];
    htmlstring = Import[htmlfile, "String"];
    (* TODO: use 'mt_excerpt' field? *)
    (* TODO: use 'enclosure' field? *)
    (* TODO: use 'dateCreated' field? *)
    (* TODO: use 'wp_slug' field? *)
    newpostid = 
    "Response" /. 
	BlogRequest["metaWeblog.newPost", 
		{
			(*  see ruleonef/public_html/wp-includes/class-wp-xmlrpc-server.php *)
			(*+ mw_newPost function *)
			1, 
			username, 
			password, 
			{
				"title" -> posttitle,
				"description" -> htmlstring,
				"categories" -> postcategories
			}, 
			True
		}, 
		FilterRules[{opts}, Options@BlogRequest]
	];
    newpostid = newpostid /. {pid_String} :> pid;
	imagefiles = FileNames["*.gif", FileNameJoin[{localexportdir, "HTMLFiles"}]];
	DebugPrint["Uploading image files ..."];
	DebugPrint[ToString@StringForm["Total images: `1`", Length@imagefiles]];
    (* upload image files *)
    With[{image = #},
		DebugPrint[image];
		DebugPrint[Framed@Import[image]];
    	"Response" /. 
    	BlogRequest[
    		"wp.uploadFile", 
    		{
    			1, 
    			username, 
    			password,
				{
					"name" -> FileNameTake[image, -1], 
					"type" -> "image/gif", 
					"bits" -> "Base64"@ExportString[Import[image, "String"], "Base64"], 
					"overwrite" -> False
				}
           }, 
           FilterRules[{opts}, Options@BlogRequest]
		]
    ]& /@ imagefiles;
    newpostlink = "http://"<>server<>If[ MatchQ[port, _Integer],":"<>IntegerString@port,""]<>"/"<>serverpath<>"?p="<>newpostid;
    DebugPrint["Post link: "];
	DebugPrint[Hyperlink@newpostlink];
	Join[{
        "PostID" -> newpostid, 
        "Link" -> Hyperlink@newpostlink
        },
        If[ (*debugQ*) False,
            {
                "HTML"->htmlstring
            },
            {}
        ]
	]
]
  
End[]

EndPackage[]

