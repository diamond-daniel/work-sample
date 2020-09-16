<?php

/*
 *  d_call_lib.php
 *  04.29.19
 *  
 *  Dan Diamond
 *  
 *  This is a library of functions to support sending requests to
 *  and receiving responses from the Indiana State 'DARMHA' server, via C# 
 *  socket server.  
 * 
 */

/*
 
This library includes, for each available DARMHA function, a wrapper function 
which:
   1) Accepts arguments appropriate for each DARMHA function. 
   2) Builds a 'request array' which contains all needed data to execute the 
   DARMHA function.  Each request array includes a tag <Action> which informs
   the socket server what it wants to do, usually which DARMHA function to call.
   3) Converts that array into an XML-compatible string.  
   4) Feeds that XML-compatible string into an available socket.  
   5) Receives a response from the DARMHA server in the form of a simple string 
   or XML-compatible string.  
   6) Ensures the response is returned in an intuitve format, either leaving it 
   as a string, or constructing appropriate arrays.  
   
Naming convention for wrapper functions is to prefix DARMHA function names with 
'darmha'.  See test harness for example function calls.  
   
When DARMHA functions take as parameters simple strings or integers, generally
the wrapper functions will take the same parameters.  

When DARMHA functions take more complicated objects as parameters in addition to 
simple strings or integers, the wrapper functions will generally absorb the simple 
string and integer parameters into a single larger array parameter.  

The socket server will parse out the fields it needs to create the necessary 
DARMHA function arguments.  Refer to the DARMHA Web Services Specification manual 
for specifics about what should be included in arguments to each function. 

Including extra fields in input arrays does no harm, apart from overhead cost of
sending unnecessary data.  

In addition to the DARMHA functions, this library also contains helper functions
which connect to and disconnect from sockets, convert arrays to XML, convert
XML back to arrays.  
  
Per instructions, this library uses "$socket_server_connection" as a global socket 
object for all functions. 
  
*/


// php's stream_get_line function expects a sentinel 
define("_SENTINEL_", "<EOF>");

////////////////////////////////////////////////////////////////////////////////
// socket functions
////////////////////////////////////////////////////////////////////////////////

// Currently thinking of $socketid as having range [1,20], identifying sockets
// This function uses a unique port for each socket ID.  

/* additional socket variables:
 
	$kickstart - did connectToSocket have to start a sleeping DARMHA interface server?	
	$process_serv - process that was opened to start server.  If ($kickstart), then 
	   disconnectFromSocket will not only disconnect from socket, but also will
	   send command to stop server.  
	   
*/
   
$kickstart = 0;
$process_serv = NULL;
$pipes = NULL;

// startServer checks to see if server is up.  If it's not, it starts it in a 
//   background process.  Called by connectToSocket.
function startServer($sockid) {
	global $socket_server_connection;
	global $pipes;
	global $kickstart;
	
		if (!is_resource($socket_server_connection)) {
			echo "Attempting to start server.\n";
			//$cmd_serv = "C:\\Users\\op97dan\\source\\repos\\synch_serv01\\synch_serv01\\bin\\Debug\\synch_serv01.exe $sockid";
			//echo "Command: " . $cmd_serv . "\n";
			$cmd_serv = ".\\synch_serv01.exe $sockid";
			$descriptor = array ( 0 => array ( "pipe", "r" ), 1 => array ( "pipe", "r" ), 2 => array ( "pipe", "w" ) );
			$p_serv = proc_open($cmd_serv, $descriptor, $pipes);
			
			if (is_resource($p_serv)) {
				echo "Server process started - port: " . $sockid + 29000 . "\n";
				$kickstart = 1;
			}	
			return $p_serv;
		}
		
	return -1;
}


// stopServer will stop the server from running whether the server required 
//   a manual start or not.  
function stopServer() {
	global $process_serv;
	global $socket_server_connection;
	global $pipes;
		
	//$return_value = proc_close($process_serv);
	$request_array = array (
	    "Action"    => "Kill");
	
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_xml($request_array, $xml_darmha_job);
	
	$request = $xml_darmha_job->asXML();		
    //fwrite($socket_server_connection, $request);
	sock_write($socket_server_connection, $request);
    $response = stream_get_line($socket_server_connection, 1200, _SENTINEL_);
	

	// still not clear to me if emptying pipes is necessary for returning full buffer.  
	// will leave in for now.  
	if (is_resource($process_serv)) {
		echo stream_get_contents($pipes[1]);
		fclose($pipes[1]);
		$return_value = proc_close($process_serv);
		echo "Closing server proc:  " . $return_value . "\n";
	}
	
	fclose($socket_server_connection);
	
	return $response;
}


function connectToSocket($socid) {
	global $socket_server_connection;
	
	$host       = "127.0.0.1";
	$port       = 29000 + $socid;
	$timeout    = 30;
	$max_tries  = 5;
	$connected  = 0;
	$fail_count = 0;	
		
	set_error_handler("warning_handler", E_WARNING);
	
	while ($connected == 0 && $fail_count < 5) {
		try {	
			$socket_server_connection = fsockopen($host,$port,$errnum,$errstr,$timeout);
			$connected = 1;
			$result = "Connected to server:  host=$host   port=$port  \n";
		} catch (Exception $e) {
			echo "Could not connect to server\n";
			$fail_count++;
			if ($fail_count == $max_tries) {
				$result = "Connection fail: " . $errnum . " " . $errstr;
			}			
			$process_serv = startServer($socid);				
		}
	}	
	
	restore_error_handler(); 	
	
	return $result;
}


function connectToSocket2($socid, $domain='qa') { //$domain='live') {
	global $socket_server_connection;
	$result = "";
	if ($domain == 'live') {
		$port_offset = 1000;
		echo "domain is: " . $domain . " and offset is: " . $port_offset . "\n";
	}
	elseif ($domain == 'qa') { 
		$port_offset = 0;
		echo "domain is: " . $domain . " and offset is: " . $port_offset . "\n";
	}
	else {
		$result = "ERROR - Invalid domain argument - ABORTING \n";
		return $result;
	}
		
	$host       = "127.0.0.1";
	$port       = 29000 + $socid + $port_offset;
	$timeout    = 30;
	$max_tries  = 5;
	$connected  = 0;
	$fail_count = 0;	
		
	set_error_handler("warning_handler", E_WARNING);
	
	while ($connected == 0 && $fail_count < 5) {
		try {	
			$socket_server_connection = fsockopen($host,$port,$errnum,$errstr,$timeout);
			$connected = 1;
			$result = "Connected to server:  host=$host   port=$port  \n";
		} catch (Exception $e) {
			echo "Could not connect to server\n";
			$fail_count++;
			if ($fail_count == $max_tries) {
				$result = "Connection fail: " . $errnum . " " . $errstr;
			}			
			$process_serv = startServer($socid + $port_offset);				
		}
	}	
	
	restore_error_handler(); 	
	
	return $result;
}



// function warning_handler, when called with set_error_handler("warning_handler", E_WARNING); 
//   turns warnings into exceptions so they can be caught with try...catch.  
//   Thanks to 'Robert', Sednev, and Vukanac: https://stackoverflow.com/questions/1241728/can-i-try-catch-a-warning
function warning_handler ($errno, $errstr) {
	throw new Exception($errstr, $errno);
}



// disconnectFromSocket disconnects from the server if it did NOT need to be 
//   started, and stops the server if the server did have to be started.  
function disconnectFromSocket() {
	global $socket_server_connection;
	global $kickstart;
	global $process_serv;
	global $pipes;
	
	if ($kickstart == 1) { 
		//echo "kickstart is: " . $kickstart . " in value\n";
		$request_array = array( 
		"Action"     => "Kill");
	} else {
		$request_array = array (
	    "Action"    => "Disconnect");
	}
	
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_xml($request_array, $xml_darmha_job);
	
	$request = $xml_darmha_job->asXML();		
	sock_write($socket_server_connection, $request);
    $response = stream_get_line($socket_server_connection, 1200, _SENTINEL_);
	
	// still not clear to me if emptying pipes is necessary for returning full buffer.  
	// will leave in for now.  
	if (is_resource($process_serv)) {
		echo stream_get_contents($pipes[1]);
		fclose($pipes[1]);
		$return_value = proc_close($process_serv);
		echo "Closing server proc:  " . $return_value . "\n";
	}
	
    fclose($socket_server_connection);
	
	$kickstart = 0;
	
	return $response;
}



////////////////////////////////////////////////////////////////////////////////
// darmha functions
////////////////////////////////////////////////////////////////////////////////


////////////////////////
// consumer functions
////////////////////////

function darmhaConsStrFixLGBTQ($cons_str) {
	$cons_str = preg_replace('/<LGB_ID_\d>/', '<LGB>', $cons_str, -1);
    $cons_str = preg_replace('/<\/LGB_ID_\d>/', '</LGB>', $cons_str, -1);
	
	return $cons_str;
}

//   ConsumerExists does not require a full consumer object as a parameter, only select fields.
//   The actual DARMHA function is called 'ConsumerExists', though the manual 
//     references 'ConsumerExist' in places.  
function darmhaConsumerExists($consumer) {
	global $socket_server_connection;
	
	$request_array = array (
	    "Action"    => "ConsumerExists",
	    "inputCons" => $consumer);
	
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_xml($request_array, $xml_darmha_job);
	
	$request = $xml_darmha_job->asXML();
	$request = darmhaConsStrFixLGBTQ($request);
	sock_write($socket_server_connection, $request);
    $response = stream_get_line($socket_server_connection, 1200, _SENTINEL_);
	
	return $response;
}

function darmhaValidateConsumer($consumer) {
	global $socket_server_connection;
	
	$request_array = array (
	    "Action"    => "ValidateConsumer",
	    "inputCons" => $consumer);
	
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_xml($request_array, $xml_darmha_job);
	
	$request = $xml_darmha_job->asXML();	
	$request = darmhaConsStrFixLGBTQ($request);
	sock_write($socket_server_connection, $request);
    $response = stream_get_line($socket_server_connection, 1200, _SENTINEL_);
	
	return $response;
}

function darmhaInsertConsumer($consumer) {
	global $socket_server_connection;
	
	$request_array = array (
	    "Action"    => "InsertConsumer",
	    "inputCons" => $consumer);
	
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_xml($request_array, $xml_darmha_job);
	
	$request = $xml_darmha_job->asXML();		
	$request = darmhaConsStrFixLGBTQ($request);
	sock_write($socket_server_connection, $request);
    $response = stream_get_line($socket_server_connection, 1200, _SENTINEL_);
	
	return $response;
}

function darmhaUpdateConsumer($consumer) {
	global $socket_server_connection;
	
	$request_array = array (
	    "Action"    => "UpdateConsumer",
	    "inputCons" => $consumer);
	
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_xml($request_array, $xml_darmha_job);
	
	$request = $xml_darmha_job->asXML();	
	sock_write($socket_server_connection, $request);
    $response = stream_get_line($socket_server_connection, 1200, _SENTINEL_);
	
	return $response;
}

function darmhaConsumerLastAssessed($id) {
	global $socket_server_connection;
	
	$request_array = array (
	    "Action"      => "ConsumerLastAssessed",
	    "Internal_ID" => $id);
	
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_xml($request_array, $xml_darmha_job);
	
	$request = $xml_darmha_job->asXML();		
	sock_write($socket_server_connection, $request);
    $response = stream_get_line($socket_server_connection, 1200, _SENTINEL_);
	
	return $response;
}

// darmhaGetConsumer returns an array which simulates a DARMHA ConsumerItem class object.
// DARMHA classes appended with 'Item' appear to simply add an Error_Code field.  
function darmhaGetConsumer($id) {
	global $socket_server_connection;
	
	$request_array = array (
	"Action"      => "GetConsumer",
	"Internal_ID" => $id);
	
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_xml($request_array, $xml_darmha_job);
	
	$request = $xml_darmha_job->asXML();
	sock_write($socket_server_connection, $request);
	$consResponse = stream_get_line($socket_server_connection, 1200, _SENTINEL_);
	
	$response = xml_to_array_2($consResponse);
		
	return $response;
}


////////////////////////
// assessment functions 
////////////////////////

function darmhaValidateAssessment($assess) {
	global $socket_server_connection;
	
	$request_array = array(
	    "Action"                 => "ValidateAssessment", 
		"Internal_ID"            => $assess['InternalID'],
		"Assessment_Date"        => $assess['AssessDate'], 
		"Clinician_ID"           => $assess['StaffID'], 
		"Assessment_Reason_Code" => $assess['ReasonCode'],
		"Answer_ID"              => $assess['Answer_ID']
		);
			
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	$ansa_xml = array_to_xml($request_array, $xml_darmha_job);
	
	$assess_str = preg_replace('/<item\d\d\d\d>/', '<answer>', $xml_darmha_job->asXML(), -1);
    $assess_str = preg_replace('/<\/item\d\d\d\d>/', '</answer>', $assess_str, -1);
	$assess_str = preg_replace('/<item\d\d\d>/', '<answer>', $assess_str, -1);
    $assess_str = preg_replace('/<\/item\d\d\d>/', '</answer>', $assess_str, -1);
		
	//	fwrite($socket_server_connection, $assess_str);
	sock_write($socket_server_connection, $assess_str);
	$response = stream_get_line($socket_server_connection, 1200, _SENTINEL_);

	return $response;	
}

// darmhaProcessAssessment functions assume the argument array will include Internal_ID
function darmhaProcessAssessment1($assess) {
	global $socket_server_connection;
	
	$request_array = array(
	    "Action"                 => "ProcessAssessment1",
		"inputAssess"            => $assess
		);

	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	$cans_xml = array_to_xml($request_array, $xml_darmha_job);
	
	$assess_str = preg_replace('/<item\d\d\d\d>/', '<answer>', $xml_darmha_job->asXML(), -1);
    $assess_str = preg_replace('/<\/item\d\d\d\d>/', '</answer>', $assess_str, -1);
	$assess_str = preg_replace('/<item\d\d\d>/', '<answer>', $assess_str, -1);
    $assess_str = preg_replace('/<\/item\d\d\d>/', '</answer>', $assess_str, -1);
	$assess_str = preg_replace('/\(/', '', $assess_str, -1);
	$assess_str = preg_replace('/\)/', '', $assess_str, -1);
	
	// XML does not like spaces in tag names
	$assess_str = str_replace(' ', '', $assess_str);
	// XML does, however, want a space between 'xml' and 'version' in declaration
	$assess_str = str_replace('xmlversion', 'xml version', $assess_str);

	sock_write($socket_server_connection, $assess_str);
	$response = stream_get_line($socket_server_connection, 1200, _SENTINEL_);

	return $response;	
		
}

// darmhaProcessAssessment functions assume the argument array will include Internal_ID
function darmhaProcessAssessment2($assess) {
	global $socket_server_connection;
	
	$request_array = array(
	    "Action"                 => "ProcessAssessment2",
		"inputAssess"            => $assess
		);

	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	$cans_xml = array_to_xml($request_array, $xml_darmha_job);
	
	$assess_str = preg_replace('/<item\d\d\d\d>/', '<answer>', $xml_darmha_job->asXML(), -1);
    $assess_str = preg_replace('/<\/item\d\d\d\d>/', '</answer>', $assess_str, -1);
	$assess_str = preg_replace('/<item\d\d\d>/', '<answer>', $assess_str, -1);
    $assess_str = preg_replace('/<\/item\d\d\d>/', '</answer>', $assess_str, -1);
	$assess_str = preg_replace('/\(/', '', $assess_str, -1);
	$assess_str = preg_replace('/\)/', '', $assess_str, -1);
	
	// XML does not like spaces in tag names
	$assess_str = str_replace(' ', '', $assess_str);
	// XML does, however, want a space between 'xml' and 'version' in declaration
	$assess_str = str_replace('xmlversion', 'xml version', $assess_str);

	sock_write($socket_server_connection, $assess_str);
	$response = stream_get_line($socket_server_connection, 1200, _SENTINEL_);

	return $response;	
		
}

// darmhaProcessAssessment functions assume the argument array will include Internal_ID
function darmhaProcessAssessment3($assess) {
	global $socket_server_connection;
	
	$request_array = array(
	    "Action"                 => "ProcessAssessment3",
		"inputAssess"            => $assess
		);

	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	$cans_xml = array_to_xml($request_array, $xml_darmha_job);

	
	$assess_str = preg_replace('/<item\d\d\d\d>/', '<answer>', $xml_darmha_job->asXML(), -1);
    $assess_str = preg_replace('/<\/item\d\d\d\d>/', '</answer>', $assess_str, -1);
	
	$assess_str = preg_replace('/<item\d\d\d>/', '<answer>', $assess_str, -1);
    $assess_str = preg_replace('/<\/item\d\d\d>/', '</answer>', $assess_str, -1);
	
	$assess_str = preg_replace('/<item\d\d>/', '<drug>', $assess_str, -1);
    $assess_str = preg_replace('/<\/item\d\d>/', '</drug>', $assess_str, -1);
	
	$assess_str = preg_replace('/<item\d>/', '<drug>', $assess_str, -1);
    $assess_str = preg_replace('/<\/item\d>/', '</drug>', $assess_str, -1);
	
	$assess_str = preg_replace('/\(/', '', $assess_str, -1);
	$assess_str = preg_replace('/\)/', '', $assess_str, -1);
	
	// XML does not like spaces in tag names
	$assess_str = str_replace(' ', '', $assess_str);
	// XML does, however, want a space between 'xml' and 'version' in declaration
	$assess_str = str_replace('xmlversion', 'xml version', $assess_str);

	sock_write($socket_server_connection, $assess_str);
	
	$response = stream_get_line($socket_server_connection, 1200, _SENTINEL_);

	return $response;	
		
}


function sock_write($connect, $req) {
	$req = $req . "<EOF>";
	
	if (strlen($req) < 6000) {
		fwrite($connect, $req);
	} 
	else {
		$i = 0;
		$pieces = str_split($req, 1024);
		foreach ($pieces as $piece) {
			echo "Piece " . $i . " is: " . $piece . "\n\n";
			fwrite($connect, $piece, strlen($piece));
			$i += 1;
		}
	}
}


function darmhaGetAssessmentResults ($Int_ID, $Assess_Date, $Int_Assess_ID) {
	global $socket_server_connection;
	
	$request_array = array(
	    "Action"                     => "GetAssessmentResults", 
		"Internal_ID"                => $Int_ID,
		"Assessment_Date"            => $Assess_Date,
        "Internal_Assessment_ID"     => $Int_Assess_ID
		);	

	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_xml($request_array, $xml_darmha_job);
	
	$request = $xml_darmha_job->asXML();
	sock_write($socket_server_connection, $request);
	$assess_resp = stream_get_line($socket_server_connection, 2200, _SENTINEL_);
	$response = xml_to_array_2($assess_resp);
	
	return $response;
}

function darmhaGetAssessmentTools () {
	global $socket_server_connection;
	
	$request_array = array (
	    "Action"              => "GetAssessmentTools"
		);
	
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_xml($request_array, $xml_darmha_job);
	
	$request = $xml_darmha_job->asXML();
	sock_write($socket_server_connection, $request);
	$response = stream_get_line($socket_server_connection, 2200, _SENTINEL_);
	
	return $response;
}


function darmhaGetAssessmentToolQuestions ($tool_id) {
	global $socket_server_connection;
	
	$request_array = array (
	    "Action"              => "GetAssessmentToolQuestions",
		"Tool_ID"             => $tool_id
		);
	
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_xml($request_array, $xml_darmha_job);
	
	$request = $xml_darmha_job->asXML();
	
	$start = microtime(true);
	sock_write($socket_server_connection, $request);
	$stop1 = microtime(true);
	$response = stream_get_line($socket_server_connection, 999999, _SENTINEL_);
	$stop2 = microtime(true);
	
	$response = xml_to_array_2($response);
	
	return $response;
}


// returns array of certifications
function darmhaGetClinicianCertification ($Internal_Staff_ID) {
	global $socket_server_connection;
	
	$request_array = array (
	    "Action"            => "GetClinicianCertification", 
		"Internal_Staff_ID"  => $Internal_Staff_ID
		);
	
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_xml($request_array, $xml_darmha_job);
	
	
	$request = $xml_darmha_job->asXML();
	
	sock_write($socket_server_connection, $request);
	$clin_cert_resp = stream_get_line($socket_server_connection, 1000, _SENTINEL_);
	$response = xml_to_array_2($clin_cert_resp);
	
	return $response;
}

// for darmhaValidateAnswers, the argument array can simply be an array of answer IDs
function darmhaValidateAnswers($assess) {
	global $socket_server_connection;
	
	$request_array = array(
	    "Action"                 => "ValidateAnswers",
		"inputAssess"            => $assess
		);

	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	$cans_xml = array_to_xml($request_array, $xml_darmha_job);
	
	$assess_str = preg_replace('/<item\d\d\d\d>/', '<answer>', $xml_darmha_job->asXML(), -1);
    $assess_str = preg_replace('/<\/item\d\d\d\d>/', '</answer>', $assess_str, -1);
	
	$assess_str = preg_replace('/<item\d\d\d>/', '<answer>', $assess_str, -1);
    $assess_str = preg_replace('/<\/item\d\d\d>/', '</answer>', $assess_str, -1);
	
	$assess_str = preg_replace('/<item\d\d>/', '<drug>', $assess_str, -1);
    $assess_str = preg_replace('/<\/item\d\d>/', '</drug>', $assess_str, -1);
	
	$assess_str = preg_replace('/<item\d>/', '<drug>', $assess_str, -1);
    $assess_str = preg_replace('/<\/item\d>/', '</drug>', $assess_str, -1);
	
	$assess_str = preg_replace('/\(/', '', $assess_str, -1);
	$assess_str = preg_replace('/\)/', '', $assess_str, -1);
	
	// XML does not like spaces in tag names
	$assess_str = str_replace(' ', '', $assess_str);
	// XML does, however, want a space between 'xml' and 'version' in declaration
	$assess_str = str_replace('xmlversion', 'xml version', $assess_str);

	sock_write($socket_server_connection, $assess_str);
	
	$response = stream_get_line($socket_server_connection, 1200, _SENTINEL_);

	return $response;	
}



////////////////////////
// episode functions
////////////////////////

function darmhaGetLatestEpisode ($Internal_ID) {
	global $socket_server_connection;
	
	$request_array = array(
	    "Action"               => "GetLatestEpisode",
		"Internal_ID"          => $Internal_ID
	    );
		
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_XML($request_array, $xml_darmha_job);
	
	$request = $xml_darmha_job->asXML();	
	sock_write($socket_server_connection, $request);
	$response = stream_get_line($socket_server_connection, 200, _SENTINEL_);
	
	return $response;
}


function darmhaGetPriorEpisode ($Internal_ID, $Int_Ep_Code, $Ep_Start_Date) {
	global $socket_server_connection;
	
	$request_array = array(
	    "Action"                => "GetPriorEpisode",
		"Internal_ID"           => $Internal_ID, 
		"Internal_Episode_Code" => $Int_Ep_Code,
        "Episode_Start_Date"    => $Ep_Start_Date		
	    );
		
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_XML($request_array, $xml_darmha_job);
	
	$request = $xml_darmha_job->asXML();	
	sock_write($socket_server_connection, $request);
	$response = stream_get_line($socket_server_connection, 200, _SENTINEL_);
	
	return $response;
}

function darmhaGetEpisodeStatus ($Internal_ID) {
	global $socket_server_connection;
	
	$request_array = array(
	    "Action"               => "GetEpisodeStatus",
		"Internal_ID"          => $Internal_ID
	    );
		
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_XML($request_array, $xml_darmha_job);
	
	$request = $xml_darmha_job->asXML();	
	sock_write($socket_server_connection, $request);
	$response = stream_get_line($socket_server_connection, 200, _SENTINEL_);
	
	return $response;
}

function darmhaGetEpisode($Internal_ID, $Internal_Episode_Code) {
	global $socket_server_connection;
	
	$request_array = array(
	    "Action"                => "GetEpisode",
		"InternalID"           => $Internal_ID, 
		"EpisodeID" => $Internal_Episode_Code
	    );
	
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_XML($request_array, $xml_darmha_job);
	
	$request = $xml_darmha_job->asXML();	
	sock_write($socket_server_connection, $request);
	$epResponse = stream_get_line($socket_server_connection, 2200, _SENTINEL_);
	$response = xml_to_array_2($epResponse);
	
	return $response;
}

function darmhaGetEpisodeList ($Internal_ID) {
	global $socket_server_connection;
	
	$request_array = array( 
	    "Action"             => "GetEpisodeList", 
		"Internal_ID"        => $Internal_ID
		);
		
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_XML($request_array, $xml_darmha_job);
		
	$request = $xml_darmha_job->asXML();
	sock_write($socket_server_connection, $request);
	$epListResponse = stream_get_line($socket_server_connection, 9999, _SENTINEL_);
	
	$response = xml_to_array_2($epListResponse);
	return $response;	
}

function darmhaCloseEpisode ($Internal_ID, $Int_Ep_Code, $Ep_End_Date, $Ep_Stat_Code, $Death_Dt="") {
	global $socket_server_connection;
	
	$request_array = array(
	    "Action"                => "CloseEpisode",
		"Internal_ID"           => $Internal_ID, 
		"Internal_Episode_Code" => $Int_Ep_Code,
        "Episode_End_Date"      => $Ep_End_Date, 
        "Episode_Status_Code"   => $Ep_Stat_Code,
        "Death_Date"	        => $Death_Dt	
	    );
		
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_XML($request_array, $xml_darmha_job);
	
	$request = $xml_darmha_job->asXML();
	sock_write($socket_server_connection, $request);
	$response = stream_get_line($socket_server_connection, 200, _SENTINEL_);
	
	return $response;
}

// darmhaInsertEpisode assumes that the argument array will include Internal_ID
function darmhaInsertEpisode($test_ep) {
	global $socket_server_connection;
	
	$request_array = array(
	    "Action"           => "InsertEpisode", 
		"inputEpisode"     => $test_ep 
		);
		
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_XML($request_array, $xml_darmha_job);
	
	$request = $xml_darmha_job->asXML();
	sock_write($socket_server_connection, $request);
	$response = stream_get_line($socket_server_connection, 200, _SENTINEL_);
	
	return $response;
}

// darmhaUpdateEpisode assumes that the argument array will include Internal_ID
function darmhaUpdateEpisode($test_ep) {
	global $socket_server_connection;
	
	$request_array = array(
	    "Action"           => "UpdateEpisode", 
		"inputEpisode"     => $test_ep 
		);
		
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_XML($request_array, $xml_darmha_job);
	
	$request = $xml_darmha_job->asXML();	
	sock_write($socket_server_connection, $request);
	$response = stream_get_line($socket_server_connection, 200, _SENTINEL_);
	
	return $response;
}


////////////////////////
// encounter functions
////////////////////////

// darmhaInsertEncounter assumes that the strings Internal_ID and 
//   Internal_Episode_Code will be included in the argument array. 
function darmhaInsertEncounter($test_enc) {
	global $socket_server_connection;
	
	$request_array = array(
	    "Action"           => "InsertEncounter", 
		"inputEncounter"   => $test_enc 
		);
		
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_XML($request_array, $xml_darmha_job);
	
	$request = $xml_darmha_job->asXML();	
	sock_write($socket_server_connection, $request);
	$response = stream_get_line($socket_server_connection, 200, _SENTINEL_);
	
	return $response;
}

// darmhaUpdateEncounter assumes that the strings Internal_ID and 
//   Internal_Episode_Code will be included in the argument array.
function darmhaUpdateEncounter($test_enc) {
	global $socket_server_connection;
	
	$request_array = array(
	    "Action"           => "UpdateEncounter", 
		"inputEncounter"   => $test_enc 
		);
		
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_XML($request_array, $xml_darmha_job);
	
	$request = $xml_darmha_job->asXML();
	sock_write($socket_server_connection, $request);
	$response = stream_get_line($socket_server_connection, 200, _SENTINEL_);
	
	return $response;
}

function darmhaDeleteEncounter($Internal_ID, $Int_Ep_Code, $Int_Serv_ID) {
	global $socket_server_connection;
	
	$request_array = array(
	    "Action"                => "DeleteEncounter",
		"Internal_ID"           => $Internal_ID, 
		"Internal_Episode_Code" => $Int_Ep_Code,
        "Internal_Service_ID"   => $Int_Serv_ID
	    );
		
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_XML($request_array, $xml_darmha_job);
	
	$request = $xml_darmha_job->asXML();
	sock_write($socket_server_connection, $request);
	$response = stream_get_line($socket_server_connection, 200, _SENTINEL_);
	
	return $response;
}

// darmhaGetEncounter returns an array which simulates the DARMHA 
//   EncounterItem class object.  
function darmhaGetEncounter($Internal_ID, $Int_Ep_Code, $Int_Serv_ID) {
	global $socket_server_connection;
	
	$request_array = array(
	    "Action"                => "GetEncounter",
		"Internal_ID"           => $Internal_ID, 
		"Internal_Episode_Code" => $Int_Ep_Code,
        "Internal_Service_ID"   => $Int_Serv_ID
	    );
		
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_XML($request_array, $xml_darmha_job);
	
	$request = $xml_darmha_job->asXML();
	sock_write($socket_server_connection, $request);
	$encResponse = stream_get_line($socket_server_connection, 1200, _SENTINEL_);
	$response = xml_to_array_2($encResponse);
		
	return $response;
}


///////////////////////////////////
// diagnosis & agreetype functions
///////////////////////////////////

function darmhaInsertDiagnosisAgreementType($diagree) {
	global $socket_server_connection;
	
	$request_array = array( 
	    "Action"                      => "InsertDiagnosisAgreementType", 
		"inputDiagnosisAgreementType" => $diagree 
		);
		
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_XML($request_array, $xml_darmha_job);
		
	$request = $xml_darmha_job->asXML();
	sock_write($socket_server_connection, $request);
	$response = stream_get_line($socket_server_connection, 200, _SENTINEL_);
	
	return $response;
}


function darmhaValidateDiagnosisCode($diag_code) {
	global $socket_server_connection;
	
	$request_array = array( 
	    "Action"                  => "ValidateDiagnosisCode", 
		"Diagnosis_Code"          => $diag_code 
		);
		
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_XML($request_array, $xml_darmha_job);
		
	$request = $xml_darmha_job->asXML();
	sock_write($socket_server_connection, $request);
	$response = stream_get_line($socket_server_connection, 200, _SENTINEL_);
	
	return $response;
}


function darmhaGetDiagnosisList() {
	global $socket_server_connection;
	
	$request_array = array( 
	    "Action"             => "GetDiagnosisList"
		);
		
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_XML($request_array, $xml_darmha_job);
		
	$request = $xml_darmha_job->asXML();
	sock_write($socket_server_connection, $request);
	$diagResponse = stream_get_line($socket_server_connection, 999999, _SENTINEL_);
	
	$response = xml_to_array_2($diagResponse);
	return $response;	
}


function darmhaGetLatestAgreementType($int_ID, $int_ep_code, $enc_date) {
	global $socket_server_connection;
	
	$request_array = array(
	    "Action"                => "GetLatestAgreementType",
		"Internal_ID"           => $int_ID, 
		"Internal_Episode_Code" => $int_ep_code,
        "Encounter_Date"        => $enc_date
	    );
		
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_XML($request_array, $xml_darmha_job);
	
	$request = $xml_darmha_job->asXML();
	sock_write($socket_server_connection, $request);
	$response = stream_get_line($socket_server_connection, 200, _SENTINEL_);
	
	return $response;
}

////////////////////////
// NOMS functions 
////////////////////////

function darmhaInsertNOMS($noms) {
	global $socket_server_connection;
	
	$request_array = array( 
	    "Action"             => "InsertNOMS", 
		"inputNOMS"          => $noms 
		);
		
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_XML($request_array, $xml_darmha_job);
		
	$request = $xml_darmha_job->asXML();
	sock_write($socket_server_connection, $request);
	$response = stream_get_line($socket_server_connection, 200, _SENTINEL_);
	
	return $response;	
}

function darmhaGetSubstanceCodes() {
	global $socket_server_connection;
	
	$request_array = array( 
	    "Action"             => "GetSubstanceCodes"
		);
		
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_XML($request_array, $xml_darmha_job);
		
	$request = $xml_darmha_job->asXML();
	sock_write($socket_server_connection, $request);
	$subsResponse = stream_get_line($socket_server_connection, 5000, _SENTINEL_);
	$subsX = simplexml_load_string($subsResponse);

	$response = xml_to_array_2($subsResponse);
	return $response;	
}


////////////////////////
// SUD functions 
////////////////////////
function darmhaInsertSUDTreatmentAdmission ($sud_ad) {
	global $socket_server_connection;
	
	$request_array = array( 
	    "Action"             => "InsertSUDAdmit", 
		"inputSUDAdmit"      => $sud_ad 
		);
		
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_XML($request_array, $xml_darmha_job);
		
	$request = $xml_darmha_job->asXML();
	sock_write($socket_server_connection, $request);
	$response = stream_get_line($socket_server_connection, 200, _SENTINEL_);
	
	return $response;
}

function darmhaUpdateSUDTreatmentAdmission ($sud_ad) {
	global $socket_server_connection;
	
	$request_array = array( 
	    "Action"             => "UpdateSUDAdmit", 
		"inputSUDAdmit"      => $sud_ad 
		);
		
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_XML($request_array, $xml_darmha_job);
		
	$request = $xml_darmha_job->asXML();
	sock_write($socket_server_connection, $request);
	$response = stream_get_line($socket_server_connection, 200, _SENTINEL_);
	
	return $response;
}

function darmhaInsertSUDTreatmentDischarge ($sud_dx) {
	global $socket_server_connection;
	
	$request_array = array( 
	    "Action"             => "InsertSUDDisch", 
		"inputSUDDisch"      => $sud_dx
		);
		
	$xml_darmha_job = new SimpleXMLElement('<?xml version="1.0"?><job></job>');
	array_to_XML($request_array, $xml_darmha_job);
		
	$request = $xml_darmha_job->asXML();
	sock_write($socket_server_connection, $request);
	$response = stream_get_line($socket_server_connection, 200, _SENTINEL_);
	
	return $response;
}



////////////////////////////////////////////////////////////////////////////////
// XML and array functions
////////////////////////////////////////////////////////////////////////////////


function xml_to_array_2($xml_str) {
	$xml = simplexml_load_string($xml_str, "SimpleXMLElement", LIBXML_NOCDATA);

	if ($xml != FALSE) {
	    $json = json_encode($xml);
	    $array = json_decode($json, TRUE);
		
	    if (!is_null($array)) {		
		$array = clean_array($array);
	    } 
	
	    return $array;
	}
	elseif (is_string($xml_str)) {
	    return $xml_str;
	}
	else {
	    return "No message received by xml_to_array_2";
	}
}


// recursively change array(0) to "" at all depths of an array
function clean_array($ar) {
	foreach($ar as $k => $v) {
		if (is_array($v)) {
			if (sizeof($v) == 0) {
				$ar[$k] = "";
			} else {
				$temp = clean_array($v);
				$ar[$k] = $temp;
			}
		}
	}
	return $ar;
}


function array_to_xml( $data, &$xml_data ) {
    foreach( $data as $key => $value ) {
        if( is_numeric($key) ){
            $key = 'item'.$key; //dealing with <0/>..<n/> issues
        }
        if( is_array($value) ) {
            $subnode = $xml_data->addChild($key);
            array_to_xml($value, $subnode);
        } else {
            $xml_data->addChild("$key",htmlspecialchars("$value"));
        }
     }
}



?>
