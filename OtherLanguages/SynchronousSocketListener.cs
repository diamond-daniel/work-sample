using synch_serv01.darm;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Runtime.Serialization;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Xml;
using System.Xml.Linq;
using System.Xml.Serialization;

namespace synch_serv01
{
    class SynchronousSocketListener
    {
        public static string request = null;
        public string logfile = null;

        public void StartListening(int socket_id, string domain)
        {
            logfile = @".\logs\log_" + DateTime.Now.ToString("yyyyMMdd_hhmmss") + "_th" + socket_id + ".txt";

            // byte array for incoming requests
            byte[] bytes = new Byte[30000];

            int port;

            if (domain == "prod")
            {
                port = 30000 + socket_id;
            }
            else
            {
                port = 29000 + socket_id;
            }

            IPAddress ipAddress = IPAddress.Parse("127.0.0.1");
            IPEndPoint localEndPoint = new IPEndPoint(ipAddress, port);

            LogMessage("Endpoint address: " + localEndPoint.Address + "   Endpoint port: " + localEndPoint.Port + "   " + DateTime.Now);

            Socket listener = new Socket(ipAddress.AddressFamily,
                SocketType.Stream, ProtocolType.Tcp);

            // listener level
            try
            {
                listener.Bind(localEndPoint);
                listener.Listen(10);
                bool stop_server = false;
                
                // Start listening for connections.  
                while (true)
                {
                    // connection level
                    try
                    {
                        if (stop_server) { break; }
                        Console.WriteLine("\nWaiting for a connection...");
                        Console.WriteLine("ipAddress: " + ipAddress + "    Port: " + port);

                        Socket handler = listener.Accept();

                        synch_serv01.darm.DARMHAServiceClient darm_soap_obj = new synch_serv01.darm.DARMHAServiceClient();
                        darm_soap_obj.ClientCredentials.UserName.UserName = "[REDACTED]";
                        darm_soap_obj.ClientCredentials.UserName.Password = "[REDACTED]";
                        darm_soap_obj.Open();

                        // A connection has been made, wait to receive requests.    
                        while (true)
                        {
                            request = null;
                            bool disconnect = false;
                            int bytesRec = 0;
                            XDocument req_xml = null;
                            string action = null;
                            string response = null;
                            bool eof_index = false;

                            Console.WriteLine("\nAwaiting request...");
                            // request level
                            // catch errors associated with client forced socket closing
                            try
                            {

                                while (!eof_index)
                                { 
                                    bytesRec = handler.Receive(bytes);
                                    request += Encoding.ASCII.GetString(bytes, 0, bytesRec);
                                    eof_index = request.Contains("<EOF>");
                                }

                                //Console.WriteLine("Size of bytes array after receiving bytes: {0}", bytes.Length);
                                Console.WriteLine("Bytes received: {0}", bytesRec);
                                Console.WriteLine("Request string received:  " + request);

                                request = request.Replace("<EOF>", ""); // can't parse XML if there are 2 roots <job> & <EOF>
                                req_xml = XDocument.Parse(request);
                                action = req_xml.Descendants("Action").Single().Value;

                                Console.WriteLine("\n###### Request received ######\n{0}", req_xml.ToString());

                            } // request level try
                              // return to waiting for connection
                            catch (System.ServiceModel.Security.MessageSecurityException mse)
                            {
                                LogMessage("Exception caught when receiving request string.\n");
                                DumpException(mse);
                                LogException(mse);

                                // no need for response because throwing this exception up a level to close connection?  
                                response = "ERROR: " + mse.GetType().Name + " when receiving request.";
                                Console.WriteLine(response);

                                throw;
                            }
                            catch (SocketException se)
                            {
                                LogMessage("Exception caught when receiving request string.\n");
                                DumpException(se);
                                LogException(se);

                                // no need for response because throwing this exception up a level to close connection?  
                                response = "ERROR: " + se.GetType().Name + " when receiving request.";
                                Console.WriteLine(response);

                                throw;
                            }
                            catch (InvalidOperationException ioe)
                            {
                                LogMessage("Exception caught when receiving request string.\n");
                                DumpException(ioe);
                                LogException(ioe);

                                response = "ERROR: " + ioe.GetType().Name + " when receiving request.";
                                Console.WriteLine(response);

                            }
                            catch (Exception ex)
                            {
                                LogMessage("Exception caught when receiving request string.\n");
                                DumpException(ex);
                                LogException(ex);

                                response = "ERROR: " + ex.GetType().Name + " when receiving request.";
                                Console.WriteLine(response);

                            }

                            ConsumerItem consResponse = null;

                            // request level
                            // call function for DARMHA action 
                            int max_attempts = 3;
                            int attempt = 1;
                            while (true)
                            {
                                try
                                {
                                    switch (action)
                                    {
                                        // Consumer functions
                                        case "ValidateConsumer":
                                            response = ValidateCons(req_xml, darm_soap_obj);
                                            break;

                                        case "ConsumerExists":
                                            response = ConsExists(req_xml, darm_soap_obj);
                                            break;

                                        case "InsertConsumer":
                                            response = InsertCons(req_xml, darm_soap_obj);
                                            break;

                                        case "UpdateConsumer":
                                            response = UpdateCons(req_xml, darm_soap_obj);
                                            break;

                                        case "ConsumerLastAssessed":
                                            response = ConsLastAssessed(req_xml, darm_soap_obj);
                                            break;

                                        case "GetConsumer":
                                            consResponse = GetCons(req_xml, darm_soap_obj);
                                            if (consResponse != null)
                                            {
                                                Console.WriteLine("Consumer Received: ");
                                                ConsumerItemDump(consResponse);
                                                response = ConsToXMLStr(consResponse);
                                            }
                                            else
                                            {
                                                response = "ERROR: Could not complete GetConsumer call.";
                                            }
                                            //Console.WriteLine("Response converted to XML string:  " + response);
                                            break;

                                        // Assessment functions
                                        case "ValidateAssessment":
                                            response = ValidateAssess(req_xml, darm_soap_obj);
                                            break;

                                        case "ProcessAssessment1":
                                            response = ProcessAssessment1(req_xml, darm_soap_obj);
                                            break;

                                        case "ProcessAssessment2":
                                            response = ProcessAssessment2(req_xml, darm_soap_obj);
                                            break;

                                        case "ProcessAssessment3":
                                            response = ProcessAssessment3(req_xml, darm_soap_obj, request);
                                            break;

                                        case "GetAssessmentResults":
                                            response = GetAssessmentResults(req_xml, darm_soap_obj);
                                            break;

                                        case "GetAssessmentTools":
                                            response = GetAssessmentTools(darm_soap_obj);
                                            break;

                                        case "GetAssessmentToolQuestions":
                                            response = GetAssessmentToolQuestions(req_xml, darm_soap_obj);
                                            break;

                                        case "GetClinicianCertification":
                                            response = GetClinicianCertification(req_xml, darm_soap_obj);
                                            break;

                                        case "ValidateAnswers":
                                            response = ValidateAnswers(req_xml, darm_soap_obj);
                                            break;

                                        // Episode functions
                                        case "GetLatestEpisode":
                                            response = GetLatestEpisode(req_xml, darm_soap_obj);
                                            break;

                                        case "GetPriorEpisode":
                                            response = GetPriorEpisode(req_xml, darm_soap_obj);
                                            break;

                                        case "GetEpisodeStatus":
                                            response = GetEpisodeStatus(req_xml, darm_soap_obj);
                                            break;

                                        case "GetEpisode":
                                            response = GetEpisode(req_xml, darm_soap_obj);
                                            break;

                                        case "GetEpisodeList":
                                            response = GetEpisodeList(req_xml, darm_soap_obj);
                                            break;

                                        case "CloseEpisode":
                                            response = CloseEpisode(req_xml, darm_soap_obj);
                                            break;

                                        case "InsertEpisode":
                                            response = InsertEpisode(req_xml, darm_soap_obj);
                                            break;

                                        case "UpdateEpisode":
                                            response = UpdateEpisode(req_xml, darm_soap_obj);
                                            break;

                                        // Encounter functions 
                                        case "InsertEncounter":
                                            response = InsertEncounter(req_xml, darm_soap_obj);
                                            break;

                                        case "UpdateEncounter":
                                            response = UpdateEncounter(req_xml, darm_soap_obj);
                                            break;

                                        case "DeleteEncounter":
                                            response = DeleteEncounter(req_xml, darm_soap_obj);
                                            break;

                                        case "GetEncounter":
                                            response = GetEncounter(req_xml, darm_soap_obj);
                                            break;

                                        // Diagnosis and Agreement Type functions
                                        case "InsertDiagnosisAgreementType":
                                            response = InsertDiagnosisAgreementType(req_xml, darm_soap_obj);
                                            break;

                                        case "ValidateDiagnosisCode":
                                            response = ValidateDiagnosisCode(req_xml, darm_soap_obj);
                                            break;

                                        case "GetDiagnosisList":
                                            response = GetDiagnosisList(req_xml, darm_soap_obj);
                                            break;

                                        case "GetLatestAgreementType":
                                            response = GetLatestAgreementType(req_xml, darm_soap_obj);
                                            break;

                                        // NOMS
                                        case "InsertNOMS":
                                            response = InsertNOMS(req_xml, darm_soap_obj);
                                            break;

                                        case "GetSubstanceCodes":
                                            response = GetSubstanceCodes(darm_soap_obj);
                                            break;

                                        // SUD
                                        case "InsertSUDAdmit":
                                            response = InsertSUDAdmit(req_xml, darm_soap_obj);
                                            break;

                                        case "UpdateSUDAdmit":
                                            response = UpdateSUDAdmit(req_xml, darm_soap_obj);
                                            break;

                                        case "InsertSUDDisch":
                                            response = InsertSUDDisch(req_xml, darm_soap_obj);
                                            break;

                                        // Disconnect
                                        case "Disconnect":
                                            response = "Server disconnecting";
                                            disconnect = true;
                                            break;

                                        case "Kill":
                                            response = "Stopping server";
                                            disconnect = true;
                                            stop_server = true;
                                            break;

                                        default:
                                            Console.WriteLine("Invalid Action");
                                            break;
                                    }
                                    break; // function executed without exception, so stop trying

                                } // request level try
                                catch (System.ServiceModel.ServerTooBusyException stb)
                                {
                                    DumpException(stb);
                                    LogException(stb, req_xml);

                                    if (attempt == max_attempts)
                                    {
                                        Console.WriteLine(stb.GetType().Name + " Try #{0} failed.  Aborting.", attempt);
                                        LogMessage(action + " Attempt #" + attempt + " failed.  Aborting.");
                                        response = "ERROR: " + stb.GetType().Name + " on " + action + " function call - failed max times.";
                                        break;
                                    }
                                    else 
                                    {
                                        Console.WriteLine(stb.GetType().Name + " Try #{0} failed.  Retrying.", attempt);
                                        LogMessage(action + " Attempt #" + attempt + " failed.  Retrying.");
                                        Thread.Sleep(5000);
                                        attempt++;
                                    }
                                }
                                catch (TimeoutException te)
                                {
                                    DumpException(te);
                                    LogException(te, req_xml);

                                    if (attempt == max_attempts)
                                    {
                                        Console.WriteLine(te.GetType().Name + " Try #{0} failed.  Aborting.", attempt);
                                        LogMessage(action + " Attempt #" + attempt + " failed.  Aborting.");
                                        response = "ERROR: " + te.GetType().Name + " on " + action + " request - failed max times.";
                                        Console.WriteLine(response);
                                        break;
                                    }
                                    else
                                    {
                                        Console.WriteLine(te.GetType().Name + " Try #{0} failed.  Retrying.", attempt);
                                        LogMessage(action + " Attempt #" + attempt + " failed.  Retrying.");
                                        Thread.Sleep(5000);
                                        attempt++;
                                    }
                                }
                                catch (System.ServiceModel.Security.MessageSecurityException mse)
                                {
                                    DumpException(mse);
                                    LogException(mse, req_xml);

                                    if (attempt == max_attempts)
                                    {
                                        Console.WriteLine(mse.GetType().Name + " Try #{0} failed.  Aborting.", attempt);
                                        LogMessage(action + " Attempt #" + attempt + " failed.  Aborting.");
                                        response = "ERROR: " + mse.GetType().Name + " on " + action + " request - failed max times.";
                                        Console.WriteLine(response);
                                        break;
                                    }
                                    else
                                    {
                                        Console.WriteLine(mse.GetType().Name + " Try #{0} failed.  Retrying.", attempt);
                                        LogMessage(action + " Attempt #" + attempt + " failed.  Retrying.");

                                        Console.WriteLine("Creating new SOAP client...");
                                        synch_serv01.darm.DARMHAServiceClient dummy_soap_obj = new synch_serv01.darm.DARMHAServiceClient();
                                        dummy_soap_obj.ClientCredentials.UserName.UserName = "wabashWS";
                                        dummy_soap_obj.ClientCredentials.UserName.Password = "h1kjEpID";
                                        dummy_soap_obj.Open();

                                        darm_soap_obj = dummy_soap_obj;
                                        Console.WriteLine("darm_soap_obj was assigned value dummy_soap_obj...continuing with retry.");

                                        Thread.Sleep(5000);
                                        attempt++;
                                    }


                                }
                                catch (SocketException se)
                                {
                                    // no need for response because throwing this exception up a level to close connection?  
                                    response = "ERROR: " + se.GetType().Name + " on " + action + " request.";
                                    LogMessage(response);
                                    LogException(se, req_xml);

                                    Console.WriteLine(response);
                                    DumpException(se);

                                    throw;
                                }
                                catch (InvalidOperationException ioe)
                                {
                                    response = "ERROR: " + ioe.GetType().Name + " on " + action + " request.  Are field names correct?";
                                    LogMessage(response);
                                    LogException(ioe, req_xml);

                                    Console.WriteLine(response);
                                    DumpException(ioe);

                                    break;
                                }
                                catch (FormatException fe)
                                {
                                    response = "ERROR: " + fe.GetType().Name + " on " + action + " request.";
                                    LogMessage(response);
                                    LogException(fe, req_xml);

                                    Console.WriteLine(response);
                                    DumpException(fe);

                                    break;
                                }
                                catch (XmlException xe)
                                {
                                    response = "ERROR: " + xe.GetType().Name + " on " + action + " request.";
                                    LogMessage(response);
                                    LogException(xe, req_xml);

                                    Console.WriteLine(response);
                                    DumpException(xe);

                                    break;
                                }
                                catch (NullReferenceException nre)
                                {
                                    response = "ERROR: " + nre.GetType().Name + " on " + action + " request.";
                                    LogMessage(response);
                                    LogException(nre, req_xml);

                                    Console.WriteLine(response);
                                    DumpException(nre);

                                    break;
                                }
                                catch (Exception ex)
                                {
                                    response = "ERROR: " + ex.GetType().Name + " on " + action + " request.";
                                    LogMessage(response);
                                    LogException(ex);

                                    Console.WriteLine(response);
                                    DumpException(ex);
                                    
                                    break;
                                } 

                            } // while trying


                            // request level
                            // Put this in a while loop if we decide we want it to try multiple times to return response message
                            // Then add breaks when reach max tries.  
                            //while (true)
                            //{
                                try
                                {
                                    // forward DARMHA's response back to caller
                                    response = response + "<EOF>"; // PHP's stream_get_line takes a sentinel
                                    byte[] msg = Encoding.ASCII.GetBytes(response);
                                    handler.Send(msg);
                                    //break;
                                } // request level try
                                catch (System.ServiceModel.Security.MessageSecurityException mse)
                                {
                                    // no need to send response because throwing this exception up a level to close connection?  
                                    response = "ERROR: " + mse.GetType().Name + " sending " + action + " response.";
                                    LogMessage(response);
                                    LogException(mse, req_xml);

                                    Console.WriteLine(response);
                                    DumpException(mse);

                                    throw;
                                }
                                catch (SocketException se)
                                {
                                    // no need for response because throwing this exception up a level to close connection?  
                                    response = "ERROR: " + se.GetType().Name + " sending " + action + " response.";
                                    LogMessage(response);
                                    LogException(se, req_xml);

                                    Console.WriteLine(response);
                                    DumpException(se);

                                    throw;
                                }
                                catch (Exception ex)
                                {
                                    response = "ERROR: " + ex.GetType().Name + " sending " + action + " response.";
                                    LogMessage(response);
                                    LogException(ex, req_xml);

                                    Console.WriteLine(response);
                                    DumpException(ex);

                                    //break;
                                }

                            //}

                            if (disconnect) { break; }
                            if (request.IndexOf("<EOF>") > -1) // this is not working correctly for disconnect?  
                            {
                                break;
                            }
                        } // end request while

                        darm_soap_obj.Close();
                        handler.Shutdown(SocketShutdown.Both);
                        handler.Close();
                    } // connection level try
                    catch(Exception ex)
                    {
                        LogMessage("\n*** Exception caught at connection level ***\n");

                        // DumpException(ex); // This gave unnecessarily scary message on server when there was a rocky soap close.
                        Console.WriteLine("*** Exception caught at connection level. Warning: Possible soap close error. ***");
                        LogException(ex);
                    }
                } // end connect while
            } // listener level try
            catch (Exception ex)
            {
                LogMessage("\n*** Exception caught at listener level ***\n");

                DumpException(ex);
                LogException(ex);
            }
       }



        /********************************************************************
         * DARMHA Functions
         *******************************************************************/
        //////////////////////////////////
        // Consumer Functions
        //////////////////////////////////
        private string ConsExists(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Consumer Exists";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            try
            { 
                string cons_exist = null;

                string Internal_ID = req_xml.Descendants("Internal_ID").SingleOrDefault().Value;
                Console.WriteLine("ConsExists Internal_ID:  " + Internal_ID + "\n");
                int Medicaid_Active = int.Parse(req_xml.Descendants("Medicaid_Active").SingleOrDefault().Value);
                string Medicaid_ID = req_xml.Descendants("MedicaidID").SingleOrDefault().Value;
                string Last_Name = req_xml.Descendants("LastName").SingleOrDefault().Value;
                string First_Name = req_xml.Descendants("FirstName").SingleOrDefault().Value;
                string Birth_Date = req_xml.Descendants("BirthDate").SingleOrDefault().Value;

                Console.WriteLine("int ID: " + Internal_ID);

                int? exist_int = darm_soap_obj.ConsumerExists(Internal_ID, Medicaid_Active,
                    Medicaid_ID, Last_Name, First_Name, Birth_Date);

                if (exist_int != null)
                {
                    cons_exist = exist_int.ToString();
                }
                else
                {
                    cons_exist = "No record of consumer";
                }
                Console.WriteLine("Result of ConsumerExists call: " + cons_exist);

                return cons_exist;
            }

            catch (SocketException se)
            {
                DumpException(se);
                LogException(call, se, req_xml);

                throw;
            }
            catch (InvalidOperationException ioe)
            {
                DumpException(ioe);
                LogException(call, ioe, req_xml);

                string err = "ERROR on " + call + " call - " + ioe.GetType().Name + " - Are field names correct?";
                return err;
            }
            catch (Exception ex)
            {
                DumpException(ex);
                LogException(call, ex, req_xml);

                string err = "ERROR: Could not complete " + call + " call.";
                return err;
            }
        }


        private string ValidateCons(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Validate Consumer";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            Consumer cons = createConsumerObj(req_xml);
            ConsumerDump(cons);
            string valid_response = darm_soap_obj.ValidateConsumer(cons);
            Console.WriteLine("Result of ValidateConsumer call: " + valid_response);

            return valid_response;

        }

        private ConsumerItem GetCons(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Get Consumer";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            string Internal_ID;
            ConsumerItem getCons;
            Internal_ID = req_xml.Descendants("Internal_ID").SingleOrDefault().Value;
            getCons = darm_soap_obj.GetConsumer(Internal_ID);

            return getCons;
        }

        private string InsertCons(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Insert Consumer";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);
            
            Consumer cons = createConsumerObj(req_xml);
            ConsumerDump(cons);
            string inserted = darm_soap_obj.InsertConsumer(cons);
            Console.WriteLine("Result of InsertConsumer call: " + inserted);

            return inserted;
        }

        private string UpdateCons(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Update Consumer";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            Consumer cons = createConsumerObj(req_xml);
            ConsumerDump(cons);
            string updated = darm_soap_obj.UpdateConsumer(cons);
            Console.WriteLine("Result of UpdateConsumer call: " + updated);

            return updated;
        }

        private string ConsLastAssessed(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Consumer Last Assessed";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            string Internal_ID = req_xml.Descendants("Internal_ID").SingleOrDefault().Value;
            string last_assessed = darm_soap_obj.ConsumerLastAssesed(Internal_ID);
            Console.WriteLine("Result of ConsumerLastAssessed call: " + last_assessed);

            return last_assessed;
        }

        //////////////////////////////////
        // Assessment Functions
        //////////////////////////////////
        private string ValidateAssess(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Validate Assessment 1";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            string Internal_ID = req_xml.Descendants("Internal_ID").SingleOrDefault().Value;
            string Assessment_Date = req_xml.Descendants("Assessment_Date").SingleOrDefault().Value;
            string Clinician_ID = req_xml.Descendants("Clinician_ID").SingleOrDefault().Value;
            int Assessment_Reason_Code = int.Parse(req_xml.Descendants("Assessment_Reason_Code").SingleOrDefault().Value);
            int[] Answer_ID = req_xml.Descendants("answer").Select(element => int.Parse(element.Value)).ToArray();

            string val_assess = darm_soap_obj.ValidateAssessment(Internal_ID,
                Assessment_Date, Clinician_ID, Assessment_Reason_Code, Answer_ID);

            return val_assess;
        }

        private string ProcessAssessment1(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Process Assessment 1";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            string Internal_ID = req_xml.Descendants("InternalID").SingleOrDefault().Value;
            Console.WriteLine("Internal ID: " + Internal_ID);
            AssessmentHeader1 assess = createAssessHead1Obj(req_xml);
            AssessHead1Dump(assess);
            string proc1 = null;

            proc1 = darm_soap_obj.ProcessAssessment1(Internal_ID, assess);
            Console.WriteLine("DARMHA Response to Process AssessHead1: " + proc1);

            return proc1;
        }

        private string ProcessAssessment2(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Process Assessment 2";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            string Internal_ID = req_xml.Descendants("InternalID").SingleOrDefault().Value;
            Console.WriteLine("Internal ID: " + Internal_ID);
            AssessmentHeader2 assess = createAssessHead2Obj(req_xml);
            AssessHead2Dump(assess);
            string proc2 = null;

            proc2 = darm_soap_obj.ProcessAssessment2(Internal_ID, assess);

            Console.WriteLine("DARMHA Response to Process AssessHead2: " + proc2);

            return proc2;
        }

        private string ProcessAssessment3(XDocument req_xml, DARMHAServiceClient darm_soap_obj, string request)
        {
            string call = "Process Assessment 3";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            int? preg = null;
            string Internal_ID = "";
            SubstanceUse[] SubListArray = null;

            try
            {
                string proc3 = null;

                List<SubstanceUse> substanceList = new List<SubstanceUse>();
                AssessmentHeader3 assess = createAssessHead3Obj(req_xml);
                AssessHead3Dump(assess);

                try
                {
                    Internal_ID = req_xml.Descendants("InternalID").SingleOrDefault().Value;
                    Console.WriteLine("Internal ID: " + Internal_ID);
                }
                catch (Exception ex)
                {
                    LogException(ex);
                }


                try
                {
                    preg = int.Parse(req_xml.Descendants("Currently_Pregnant").Single().Value);
                }
                catch (Exception ex)
                {
                    LogException(ex);
                }

                try
                {
                    foreach (XElement xe in req_xml.Descendants("drug"))
                    {
                        int? op = null;
                        int? op_help = null;
                        int? intense = null;
                        int? intense_help = null;
                        int? ph = null;
                        int? ph_help = null;
                        int? mt = null;
                        int? mt_help = null;
                        int? res = null;
                        int? res_help = null;
                        int? ip = null;
                        int? ip_help = null;
                        int? rt = null;
                        int? freq = null;
                        int? twenty4 = null;

                        try
                        {
                            if (xe.Descendants("Outpatient").Single().Value != null)
                            {
                                op = int.Parse(xe.Descendants("Outpatient").Single().Value);
                            }
                        }
                        catch (Exception ex)
                        {
                            LogException(ex);
                        }

                        try
                        {
                            if (xe.Descendants("Helpful_Outpatient").Single().Value != null)
                            {
                                op_help = int.Parse(xe.Descendants("Helpful_Outpatient").Single().Value);
                            }
                        }
                        catch (Exception ex)
                        {
                            LogException(ex);
                        }

                        try
                        {
                            if (xe.Descendants("Intensive").Single().Value != null)
                            {
                                intense = int.Parse(xe.Descendants("Intensive").Single().Value);
                            }
                        }
                        catch (Exception ex)
                        {
                            LogException(ex);
                        }

                        try
                        {
                            if (xe.Descendants("Helpful_Intensive").Single().Value != null)
                            {
                                intense_help = int.Parse(xe.Descendants("Helpful_Intensive").Single().Value);
                            }
                        }
                        catch (Exception ex)
                        {
                            LogException(ex);
                        }

                        try
                        {
                            if (xe.Descendants("Partial_Hospitalization").Single().Value != null)
                            {
                                ph = int.Parse(xe.Descendants("Partial_Hospitalization").Single().Value);
                            }
                        }
                        catch (Exception ex)
                        {
                            LogException(ex);
                        }

                        try
                        {
                            if (xe.Descendants("Helpful_Partial_Hospitalization").Single().Value != null)
                            {
                                ph_help = int.Parse(xe.Descendants("Helpful_Partial_Hospitalization").Single().Value);
                            }
                        }
                        catch (Exception ex)
                        {
                            LogException(ex);
                        }

                        try
                        {
                            if (xe.Descendants("MAT").Single().Value != null)
                            {
                                mt = int.Parse(xe.Descendants("MAT").Single().Value);
                            }
                        }
                        catch (Exception ex)
                        {
                            LogException(ex);
                        }

                        try
                        {
                            if (xe.Descendants("Helpful_MAT").Single().Value != null)
                            {
                                mt_help = int.Parse(xe.Descendants("Helpful_MAT").Single().Value);
                            }
                        }
                        catch (Exception ex)
                        {
                            LogException(ex);
                        }

                        try
                        {
                            if (xe.Descendants("Residential").Single().Value != null)
                            {
                                res = int.Parse(xe.Descendants("Residential").Single().Value);
                            }
                        }
                        catch (Exception ex)
                        {
                            LogException(ex);
                        }

                        try
                        {
                            if (xe.Descendants("Helpful_Residential").Single().Value != null)
                            {
                                res_help = int.Parse(xe.Descendants("Helpful_Residential").Single().Value);
                            }
                        }
                        catch (Exception ex)
                        {
                            LogException(ex);
                        }

                        try
                        {
                            if (xe.Descendants("Inpatient").Single().Value != null)
                            {
                                ip = int.Parse(xe.Descendants("Inpatient").Single().Value);
                            }
                        }
                        catch (Exception ex)
                        {
                            LogException(ex);
                        }

                        try
                        {
                            if (xe.Descendants("Helpful_Inpatient").Single().Value != null)
                            {
                                ip_help = int.Parse(xe.Descendants("Helpful_Inpatient").Single().Value);
                            }
                        }
                        catch (Exception ex)
                        {
                            LogException(ex);
                        }


                        try
                        {
                            if (xe.Descendants("Route").Single().Value != null)
                            {
                                 rt = int.Parse(xe.Descendants("Route").Single().Value);
                            }
                        }
                        catch (Exception ex)
                        {
                            LogException(ex);
                        }

                        try
                        {
                            if (xe.Descendants("Frequency").Single().Value != null)
                            {
                                 freq = int.Parse(xe.Descendants("Frequency").Single().Value);
                            }
                        }
                        catch (Exception ex)
                        {
                            LogException(ex);
                        }

                        try
                        {
                            if (xe.Descendants("TwentyFourHour_Use").Single().Value != null)
                            {
                                twenty4 = int.Parse(xe.Descendants("TwentyFourHour_Use").Single().Value);
                            }
                        }
                        catch (Exception ex)
                        {
                            LogException(ex);
                        }

                        substanceList.Add(new SubstanceUse()
                        {
                            Substance = int.Parse(xe.Element("Substance").Value),
                            Currently_Used = int.Parse(xe.Element("Currently_Used").Value),
                            Outpatient = op, 
                            Helpful_Outpatient = op_help, 
                            Intensive = intense,
                            Helpful_Intensive = intense_help,
                            Partial_Hospitalization = ph, 
                            Helpful_Partial_Hospitalization = ph_help, 
                            MAT = mt,
                            Helpful_MAT = mt_help,
                            Residential = res,
                            Helpful_Residential = res_help,
                            Inpatient = ip,
                            Helpful_Inpatient = ip_help, 
                            Route = rt,
                            Frequency = freq,
                            Twenty_FourHour_Use = twenty4
                        }
                        );
                    }

                    SubstanceListDump(substanceList);

                    SubListArray = substanceList.ToArray();
                }
                catch (SocketException se)
                {
                    DumpException(se);
                    LogException(call, se, req_xml);

                    throw;
                }
                catch (InvalidOperationException ioe)
                {
                    DumpException(ioe);
                    LogException(call, ioe, req_xml);

                    string err = "ERROR on " + call + " call - " + ioe.GetType().Name + " - Are field names correct?";
                    return err;
                }
                catch (Exception ex)
                {
                    DumpException(ex);
                    LogException(call, ex, req_xml);

                    string err = "ERROR: Could not complete " + call + " call.";
                    return err;
                }

                proc3 = darm_soap_obj.ProcessAssessment3(Internal_ID, assess, preg, SubListArray);
                Console.WriteLine("DARMHA Response to Process AssessHead3: " + proc3);

                return proc3;
            }
            catch (SocketException se)
            {
                DumpException(se);
                LogException(call, se, req_xml);

                throw;
            }
            catch (InvalidOperationException ioe)
            {
                DumpException(ioe);
                LogException(call, ioe, req_xml);

                string err = "ERROR on " + call + " call - " + ioe.GetType().Name + " - Are field names correct?";
                return err;
            }
            catch (Exception ex)
            {
                DumpException(ex);
                LogException(call, ex, req_xml);

                string err = "ERROR: Could not complete " + call + " call.";
                return err;
            }
        }

        private string GetAssessmentResults(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Get Assessment Results";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            string Internal_ID = req_xml.Descendants("Internal_ID").Single().Value;
            string Assessment_Date = req_xml.Descendants("Assessment_Date").Single().Value;
            string Internal_Assessment_ID = req_xml.Descendants("Internal_Assessment_ID").Single().Value;

            AssessmentResultItem[] assess_item_list =
                darm_soap_obj.GetAssessmentResults(Internal_ID, Assessment_Date, Internal_Assessment_ID);

            string response = AssessResultItemsToXMLStr(assess_item_list);

            Console.WriteLine(response);

            return response;
        }

        private string GetAssessmentTools(DARMHAServiceClient darm_soap_obj)
        {
            string call = "Get Assessment Tools";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            AssessmentToolItem[] tool_array = darm_soap_obj.GetAssessmentTools();
            string toolstr = ToolsToXMLStr(tool_array);
            return toolstr;
        }

        // NOTE: for this function, an attribute 'maxReceivedMessageSize=' had to be added to the App.config
        // file in the binding tag.  There were too many questions and it exceeded the default max length.  
        private string GetAssessmentToolQuestions(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Get Assessment Tool Questions";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            string questions = null;
            int Tool_ID = int.Parse(req_xml.Descendants("Tool_ID").SingleOrDefault().Value);

            AssessmentToolQuestionItem[] q_array = darm_soap_obj.GetAssessmentToolQuestions(Tool_ID);
            questions = questionsToXMLStr(q_array);
                
            return questions;
        }

        // Note:  DARMHA's GetClinicianCertification function returns null if the clinician ID has no
        // certifications.  
        private string GetClinicianCertification(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Get Clinician Certification";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            string clin_cert_str = null;
            string Internal_Staff_ID = req_xml.Descendants("Internal_Staff_ID").Single().Value;

            ClinicianCertificationItem[] clin_cert = null;
            clin_cert = darm_soap_obj.GetClinicianCertification(Internal_Staff_ID);

            if (clin_cert != null)
            {
                clin_cert_str = ClinCertToXMLStr(clin_cert);
            }
            else
            {
                XDocument ClinCertsXML = new XDocument(
                new XDeclaration("1.0", "utf-8", "yes"),
                new XElement("ClinCerts"));

                ClinCertsXML.Root.Add(
                        new XElement("clincert",
                            new XElement("Cert", "None")
                            ));

                clin_cert_str = ClinCertsXML.ToString();
            }

            return clin_cert_str;
        }

        private string ValidateAnswers(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Validate Answers";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            int[] Answer_ID = req_xml.Descendants("answer").Select(element => int.Parse(element.Value)).ToArray();

            string val_ans = darm_soap_obj.ValidateAnswers(Answer_ID);
            Console.WriteLine("Result of ValidateAnswer call: " + val_ans);
            return val_ans;
        }

        //////////////////////////////////
        // Episode Functions
        //////////////////////////////////
        private string GetLatestEpisode(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Get Latest Episode";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            string Internal_ID = req_xml.Descendants("Internal_ID").SingleOrDefault().Value;
            string getLatestEp = darm_soap_obj.GetLatestEpisode(Internal_ID);

            return getLatestEp;
        }

        private string GetPriorEpisode(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Get Prior Episode";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            string Internal_ID = req_xml.Descendants("Internal_ID").SingleOrDefault().Value;
            string Int_Ep_Code = req_xml.Descendants("Internal_Episode_Code").SingleOrDefault().Value;
            string Ep_Start = req_xml.Descendants("Episode_Start_Date").SingleOrDefault().Value;

            string prior_ep = darm_soap_obj.GetPriorEpisode(Internal_ID, Int_Ep_Code, Ep_Start);

            return prior_ep;
        }

        private string GetEpisodeStatus(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Get Episode Status";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

           string Internal_ID = req_xml.Descendants("Internal_ID").SingleOrDefault().Value;
           string getEp = darm_soap_obj.GetEpisodeStatus(Internal_ID);

           return getEp;
        }

        private string GetEpisode(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Get Episode";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            string episode_xml_str = null;
            string Internal_ID = req_xml.Descendants("InternalID").Single().Value;
            string Int_Ep_Code = req_xml.Descendants("EpisodeID").Single().Value;

            EpisodeItem ep = darm_soap_obj.GetEpisode(Internal_ID, Int_Ep_Code);
            EpisodeDump(ep);
            episode_xml_str = EpisodeItemToXMLStr(ep);

            Console.WriteLine(episode_xml_str);

            return episode_xml_str;
        }


        // Not tested
        private string GetEpisodeList(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Get Episode List";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            string Internal_ID = req_xml.Descendants("Internal_ID").Single().Value;
            EpisodeItem[] ep_list = darm_soap_obj.GetEpisodeList(Internal_ID);

            string response = EpisodeItemListToXMLStr(ep_list);
            Console.WriteLine(response);

            return response;
        }

        private string CloseEpisode(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Close Episode";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);


            string Internal_ID = req_xml.Descendants("Internal_ID").SingleOrDefault().Value;
            string Int_Ep_Code = req_xml.Descendants("Internal_Episode_Code").SingleOrDefault().Value;
            string Ep_End = req_xml.Descendants("Episode_End_Date").SingleOrDefault().Value;
            int Ep_Stat_Code = int.Parse(req_xml.Descendants("Episode_Status_Code").SingleOrDefault().Value);
            // DARMHA spec manual fails to mention that this function takes a 5th argument:  Death_Dt
            string Death_Dt = req_xml.Descendants("Death_Date").SingleOrDefault().Value;

            string close_ep = darm_soap_obj.CloseEpisode(Internal_ID, Int_Ep_Code, Ep_End, Ep_Stat_Code, Death_Dt);

            return close_ep;
        }

        private string InsertEpisode(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Insert Episode";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            string Internal_ID = req_xml.Descendants("InternalID").Single().Value;
            Episode ep = createEpisodeObj(req_xml);
            EpisodeDump(ep);
            string response = darm_soap_obj.InsertEpisode(Internal_ID, ep);

            return response;
        }

        private string UpdateEpisode(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Update Episode";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            string Internal_ID = req_xml.Descendants("InternalID").Single().Value;
            Episode ep = createEpisodeObj(req_xml);
            EpisodeDump(ep);
            string response = darm_soap_obj.UpdateEpisode(Internal_ID, ep);

            return response;
        }

        //////////////////////////////////       
        // Encounter Functions
        //////////////////////////////////
        private string InsertEncounter(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Insert Encounter";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            string Internal_ID = req_xml.Descendants("Internal_ID").Single().Value;
            string Int_Ep_Code = req_xml.Descendants("Internal_Episode_Code").Single().Value;
            Encounter enc = createEncounterObj(req_xml);

            string response = darm_soap_obj.InsertEncounter(Internal_ID, Int_Ep_Code, enc);
            return response;
        }

        private string UpdateEncounter(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Update Encounter";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            string Internal_ID = req_xml.Descendants("Internal_ID").Single().Value;
            Console.WriteLine("ConsExists Internal_ID:  " + Internal_ID + "\n");
            string Int_Ep_Code = req_xml.Descendants("Internal_Episode_Code").Single().Value;
            Console.WriteLine("ConsExists Int_Ep_Code:  " + Int_Ep_Code + "\n");

            Encounter enc = createEncounterObj(req_xml);
            EncounterDump(enc);
            string response = darm_soap_obj.UpdateEncounter(Internal_ID, Int_Ep_Code, enc);
            return response;
        }

        private string DeleteEncounter(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Delete Encounter";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            string Internal_ID = req_xml.Descendants("Internal_ID").Single().Value;
            string Int_Ep_Code = req_xml.Descendants("Internal_Episode_Code").Single().Value;
            string Int_Serv_ID = req_xml.Descendants("Internal_Service_ID").Single().Value;

            string response = darm_soap_obj.DeleteEncounter(Internal_ID, Int_Ep_Code, Int_Serv_ID);
            return response;
        }

        private string GetEncounter(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Get Encounter";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            string Internal_ID = req_xml.Descendants("Internal_ID").Single().Value;
            string Int_Ep_Code = req_xml.Descendants("Internal_Episode_Code").Single().Value;
            string Int_Serv_ID = req_xml.Descendants("Internal_Service_ID").Single().Value;

            EncounterItem enc = darm_soap_obj.GetEncounter(Internal_ID, Int_Ep_Code, Int_Serv_ID);
            EncounterDump(enc);
            string response = EncounterItemToXMLStr(enc);

            return response;
        }

        //////////////////////////////////
        // Diag and Agree Type Functions
        //////////////////////////////////
        private string InsertDiagnosisAgreementType(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Insert Diagnosis Agreement Type";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            string Internal_ID = req_xml.Descendants("InternalID").Single().Value;
            string Int_Ep_Code = req_xml.Descendants("EpisodeID").Single().Value;

            DiagnosisAgreementType diagree = createDiagAgreeTypeObj(req_xml);

            string response = darm_soap_obj.InsertDiagnosisAgreementType(Internal_ID, Int_Ep_Code, diagree);
            return response;
        }

        private string ValidateDiagnosisCode(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Validate Diagnosis Code";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            string Diagnosis_Code = req_xml.Descendants("Diagnosis_Code").Single().Value;
            string response = darm_soap_obj.ValidateDiagnosisCode(Diagnosis_Code).ToString();

            return response;
        }

        private string GetDiagnosisList(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Get Diagnosis List";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            DiagnosisCodeItem[] diagnoses = darm_soap_obj.GetDiagnosisList();
            string response = DiagItemsToXMLStr(diagnoses);

            Console.WriteLine(response);

            return response;
        }

        private string GetLatestAgreementType(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Get Latest Agreement Type";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            string Internal_ID = req_xml.Descendants("Internal_ID").Single().Value;
            string Int_Ep_Code = req_xml.Descendants("Internal_Episode_Code").Single().Value;
            string Enc_Date = req_xml.Descendants("Encounter_Date").Single().Value;

            string response = darm_soap_obj.GetLatestAgreementType(Internal_ID, Int_Ep_Code, Enc_Date).ToString();

            return response;
        }

        //////////////////////////////////
        // NOMS Functions
        //////////////////////////////////
        private string InsertNOMS(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "Insert NOMS";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            string Internal_ID = req_xml.Descendants("InternalID").Single().Value;
            Console.WriteLine("Internal_ID: " + Internal_ID);
            string Int_Ep_Code = req_xml.Descendants("EpisodeID").Single().Value;
            Console.WriteLine("EpisodeID: " + Int_Ep_Code);

            NOMS noms = createNOMSObj(req_xml);

            string response = darm_soap_obj.InsertNOMS(Internal_ID, Int_Ep_Code, noms);

            Console.WriteLine("Result of InsertNOMS call: " + response + "\n\n");
            return response;
        }

        private string GetSubstanceCodes(DARMHAServiceClient darm_soap_obj)
        {
            string call = "Get Substance Codes";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            SubstanceItem[] substances = darm_soap_obj.GetSubstanceCodes();
            string response = SubsItemsToXMLStr(substances);

            Console.WriteLine(response);

            return response;
        }


        //////////////////////////////////
        // SUD Functions
        //////////////////////////////////

        private string InsertSUDAdmit(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "InsertSUDAdmit";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            string Internal_ID = req_xml.Descendants("InternalID").SingleOrDefault().Value;
            string Internal_Episode_Code = req_xml.Descendants("EpisodeID").SingleOrDefault().Value;

            SUDTreatmentAdmission SUD_Ad = createSUDTreatmentAdmissionObj(req_xml);

            string inserted = darm_soap_obj.InsertSUDTreatmentAdmission(Internal_ID, Internal_Episode_Code, SUD_Ad);
            Console.WriteLine("Result of InsertSUDTreatmentAdmission call: " + inserted);

            return inserted;
        }


        private string UpdateSUDAdmit(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "UpdateSUDAdmit";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);

            string Internal_ID = req_xml.Descendants("InternalID").SingleOrDefault().Value;
            string Internal_Episode_Code = req_xml.Descendants("EpisodeID").SingleOrDefault().Value;

            SUDTreatmentAdmission SUD_Ad = createSUDTreatmentAdmissionObj(req_xml);

            string updated = darm_soap_obj.UpdateSUDTreatmentAdmission(Internal_ID, Internal_Episode_Code, SUD_Ad);
            Console.WriteLine("Result of UpdateSUDTreatmentAdmission call: " + updated);

            return updated;
        }
        

        private string InsertSUDDisch(XDocument req_xml, DARMHAServiceClient darm_soap_obj)
        {
            string call = "InsertSUDDischarge";
            Console.WriteLine("*** Calling Function: " + call + " ***  {0}", DateTime.Now);
            
            string Internal_ID = req_xml.Descendants("InternalID").SingleOrDefault().Value;
            string Internal_Episode_Code = req_xml.Descendants("EpisodeID").SingleOrDefault().Value;

            SUDTreatmentDischarge SUD_Dx = createSUDTreatmentDischargeObj(req_xml);

            string inserted = darm_soap_obj.InsertSUDTreatmentDischarge(Internal_ID, Internal_Episode_Code, SUD_Dx);
            Console.WriteLine("Result of InsertSUDTreatmentDischarge call: " + inserted);

            return inserted;
        }

        /********************************************************************
         * Object Instantiators
         *******************************************************************/
        private Consumer createConsumerObj(XDocument req_xml)
        {
            string medid = "";
            string docid = "";
            string socsec = "";
            string mommaiden = "";
            //string[] lgbtq_id = new string[] { "P", "L" };
            string[] lgbtq_id = new string[8];

            // /*
            try
            {
                if (req_xml.Descendants("LGBTQ_Consumer_Identity").Single().Value != null)
                {
                    lgbtq_id = req_xml.Descendants("LGBTQ_Consumer_Identity").Select(element => element.Value).ToArray();
                }
            } 
            catch (Exception ex)
            {
                LogException(ex);
            } // */

            try
            {
                if (req_xml.Descendants("SSN").Single().Value != null)
                {
                    socsec = req_xml.Descendants("SSN").Single().Value;
                }
            }
            catch (Exception ex)
            {
                LogException(ex);
            }
            
            try
            {
                if (req_xml.Descendants("MomMaiden").Single().Value != null)
                {
                    mommaiden = req_xml.Descendants("MomMaiden").Single().Value;
                }
            }
            catch (Exception ex)
            {
                LogException(ex);
            }
            
            try
            { 
                if (req_xml.Descendants("MedicaidID").Single().Value != null)
                {
                    medid = req_xml.Descendants("MedicaidID").Single().Value;
                }
            }
            catch (Exception ex)
            {
                LogException(ex);
                medid = "";
            }
            
            try
            {
                if (req_xml.Descendants("CorrectionsID").Single().Value != null)
                {
                    docid = req_xml.Descendants("CorrectionsID").Single().Value;
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine("DOCID not found.  Setting doc ID to null string");
                LogException(ex);
                docid = "";
            }

            Consumer cons = new Consumer()
            {
                Birth_Date = req_xml.Descendants("BirthDate").Single().Value,
                Sex_Birth = req_xml.Descendants("Sex_Birth").Single().Value, // FY2021 rename Birth_Gender to Sex_Birth                
                Gender_Identity = req_xml.Descendants("Gender_Identity").Single().Value,  // FY2021 rename Consumer_Gender to Gender_Identity
                Gender_Self_Describe = req_xml.Descendants("Gender_Self_Describe").Single().Value, // FY2021 add
                LGBTQ_Member = int.Parse(req_xml.Descendants("LGBTQ_Member").Single().Value), // FY2021 add
                LGBTQ_Consumer_Identity = req_xml.Descendants("LGB").Select(element => element.Value).ToArray(),  // FY2021 add
                LGBTQ_Self_Describe = req_xml.Descendants("LGBTQ_Self_Describe").Single().Value, // FY2021 add
                English_Fluency = int.Parse(req_xml.Descendants("English").Single().Value),
                Ethnicity = int.Parse(req_xml.Descendants("Ethnicity").Single().Value),
                First_Name = req_xml.Descendants("FirstName").Single().Value,
                Internal_ID = req_xml.Descendants("Internal_ID").Single().Value, // NOT IN sample array!
                Last_Name = req_xml.Descendants("LastName").Single().Value,
                Middle_Name = req_xml.Descendants("MiddleName").Single().Value, 
                Medicaid_ID = medid,
                Primary_Language = int.Parse(req_xml.Descendants("PrimLang").Single().Value),
                Race_African_American = int.Parse(req_xml.Descendants("Black").Single().Value),
                Race_American_Indian = int.Parse(req_xml.Descendants("NativeAm").Single().Value),
                Race_Asian = int.Parse(req_xml.Descendants("Asian").Single().Value),
                Race_Caucasian = int.Parse(req_xml.Descendants("White").Single().Value),
                Race_Hawaiian = int.Parse(req_xml.Descendants("Hawaiian").Single().Value),
                Race_Other = int.Parse(req_xml.Descendants("OtherRace").Single().Value),
                SSN = socsec,
                Suffix_ID = int.Parse(req_xml.Descendants("SuffixID").Single().Value),
                Mother_Maiden_Name = mommaiden,
                DOC_ID = docid,
            };

            //ConsumerDump(cons);
            return cons;

        }

        // Not tested - check spellings of array keys!!!!!!!!!!!
        private AssessmentHeader1 createAssessHead1Obj(XDocument req_xml)
        {
            AssessmentHeader1 cans0to5 = new AssessmentHeader1()
            {
                Clinician_ID = req_xml.Descendants("StaffID").Single().Value,
                Assessment_Date = req_xml.Descendants("AssessDate").Single().Value,
                Assessment_Reason_Code = int.Parse(req_xml.Descendants("ReasonCode").Single().Value),
                Internal_Episode_Code = req_xml.Descendants("EpisodeID").Single().Value,
                Internal_Assessment_ID = req_xml.Descendants("AssessID").Single().Value,

                Strengthening_Families = int.Parse(req_xml.Descendants("Strengthening_Families").Single().Value),
                Parent_Child_Interactive = int.Parse(req_xml.Descendants("Parent_Child_Interactive").Single().Value),
                Child_Parent_Psychotherapy = int.Parse(req_xml.Descendants("Child_Parent_Psychotherapy").Single().Value), // correct descendant name?
                Incredible_Years = int.Parse(req_xml.Descendants("Incredible_Years").Single().Value),
                Healthy_Families_America = int.Parse(req_xml.Descendants("").Single().Value), // FY2021 add
                Reporting_Field1 = int.Parse(req_xml.Descendants("Reporting_Field1").Single().Value),
                Reporting_Field2 = int.Parse(req_xml.Descendants("Reporting_Field2").Single().Value),
                Answer_ID = req_xml.Descendants("answer").Select(element => int.Parse(element.Value)).ToArray()
            };

            return cans0to5;
        }


        private AssessmentHeader2 createAssessHead2Obj(XDocument req_xml)
        {
            AssessmentHeader2 cans5to17 = new AssessmentHeader2()
            {
                Clinician_ID = req_xml.Descendants("StaffID").Single().Value,
                Assessment_Date = req_xml.Descendants("AssessDate").Single().Value,
                Assessment_Reason_Code = int.Parse(req_xml.Descendants("ReasonCode").Single().Value),
                Internal_Episode_Code = req_xml.Descendants("EpisodeID").Single().Value,
                Internal_Assessment_ID = req_xml.Descendants("AssessID").Single().Value,
                Trauma_Focused = int.Parse(req_xml.Descendants("Trauma_Focused").Single().Value),
                Aggression_Replacement = int.Parse(req_xml.Descendants("Aggression_Replacement").Single().Value),
                Cannabis_Youth = int.Parse(req_xml.Descendants("Cannabis_Youth").Single().Value),
                Strengthening_Families = int.Parse(req_xml.Descendants("Strengthening_Families").Single().Value),
                Parent_Child_Interactive = int.Parse(req_xml.Descendants("Parent_Child_Interactive").Single().Value),
                // note discrepancy:  Structure != Structured  :
                Structure_Psychotherapy = int.Parse(req_xml.Descendants("Structured_Psychotherapy").Single().Value),
                Dialectical_Behavior = int.Parse(req_xml.Descendants("Dialectical_Behavior").Single().Value),
                Cognitive_Behavior = int.Parse(req_xml.Descendants("Cognitive_Behavior").Single().Value),
                Incredible_Years = int.Parse(req_xml.Descendants("Incredible_Years").Single().Value),
                Functional_Family = int.Parse(req_xml.Descendants("Functional_Family").Single().Value),
                Alternative_Families = int.Parse(req_xml.Descendants("Alternative_Families").Single().Value),
                Motivational_Interviewing = int.Parse(req_xml.Descendants("Motivational_Interviewing").Single().Value),
                Cognitive_Behavior_Therapy = int.Parse(req_xml.Descendants("Cognitive_Behavior_Therapy").Single().Value),
                Multisystemic_Therapy = int.Parse(req_xml.Descendants("MultisystemicTherapy").Single().Value),
                Transition_Independence_Process =
                    int.Parse(req_xml.Descendants("TransitiontoIndependenceProcessTIP").Single().Value),
                CMHW = int.Parse(req_xml.Descendants("CMHW").Single().Value),
                CMHI = int.Parse(req_xml.Descendants("CMHI").Single().Value), // FY2021 add
                Reporting_Field1 = int.Parse(req_xml.Descendants("Reporting_Field1").Single().Value),
                Reporting_Field2 = int.Parse(req_xml.Descendants("Reporting_Field2").Single().Value),
                Answer_ID = req_xml.Descendants("answer").Select(element => int.Parse(element.Value)).ToArray()
            };


            return cans5to17;
        }

        private AssessmentHeader3 createAssessHead3Obj(XDocument req_xml)
        {
            AssessmentHeader3 ansa = new AssessmentHeader3()
            {
                Clinician_ID = req_xml.Descendants("StaffID").Single().Value,
                Assessment_Date = req_xml.Descendants("AssessDate").Single().Value,
                Assessment_Reason_Code = int.Parse(req_xml.Descendants("ReasonCode").Single().Value),
                Internal_Episode_Code = req_xml.Descendants("EpisodeID").Single().Value,
                Internal_Assessment_ID = req_xml.Descendants("AssessID").Single().Value,
                ACT_Indicator = int.Parse(req_xml.Descendants("ACTTeam").Single().Value),
                Illness_Management = int.Parse(req_xml.Descendants("IMR").Single().Value),
                Integrated_Dual_Diagnosis = int.Parse(req_xml.Descendants("IDDT").Single().Value),
                Motivational_Interviewing = int.Parse(req_xml.Descendants("MotivationalInterview").Single().Value),
                Cognitive_Behavior = int.Parse(req_xml.Descendants("CogBehaveTherp").Single().Value),
                Matrix_Model = int.Parse(req_xml.Descendants("MatrixModel").Single().Value),
                Dialectical_Behavior = int.Parse(req_xml.Descendants("DBT").Single().Value),
                Clubhouse = int.Parse(req_xml.Descendants("Clubhouse").Single().Value),
                Peer_Support = int.Parse(req_xml.Descendants("PeerSupport").Single().Value),
                Family_Psychoeducation = int.Parse(req_xml.Descendants("FamilyPsychoEd").Single().Value),
                Medication_Management = int.Parse(req_xml.Descendants("MedManagement").Single().Value),
                Supported_Employment = int.Parse(req_xml.Descendants("SuppEmp").Single().Value),
                Supported_Housing = int.Parse(req_xml.Descendants("SuppHousing").Single().Value),
                Reporting_Field1 = int.Parse(req_xml.Descendants("Reporting_Field1").Single().Value),
                Reporting_Field2 = int.Parse(req_xml.Descendants("Reporting_Field2").Single().Value),
                Answer_ID = req_xml.Descendants("answer").Select(element => int.Parse(element.Value)).ToArray()
            };

            return ansa;
        }


        private Episode createEpisodeObj(XDocument req_xml)
        {
            Episode ep = new Episode()
            {
                
                Internal_Episode_Code = req_xml.Descendants("EpisodeID").Single().Value,
                // manual refers to these dates as 'Episode_xxx_Date' 
                // manual shows these as type Date.  they are actually strings
                // note:  needs "MM/DD/YYYY"
                Start_Date = req_xml.Descendants("StartDate").Single().Value, 
                End_Date = req_xml.Descendants("EndDate").Single().Value,
                Medicaid_Active = int.Parse(req_xml.Descendants("Medicaid").Single().Value),
                Marital_Status = int.Parse(req_xml.Descendants("Marital").Single().Value),
                County = int.Parse(req_xml.Descendants("County").Single().Value),
                FoodStamps = int.Parse(req_xml.Descendants("Foodstamps").Single().Value),
                Referral_Source = int.Parse(req_xml.Descendants("Referral").Single().Value),
                Legal_Basis = int.Parse(req_xml.Descendants("LegalBasis").Single().Value),
                Military_Served = int.Parse(req_xml.Descendants("MilServed").Single().Value),
                Military_Veteran = int.Parse(req_xml.Descendants("MilVeteran").Single().Value), 
                Military_Deployed = int.Parse(req_xml.Descendants("MilDeployed").Single().Value),
                Military_Combat = int.Parse(req_xml.Descendants("MilCombat").Single().Value),
                Military_Family = int.Parse(req_xml.Descendants("MilFamily").Single().Value),
                Consumer_Disability = int.Parse(req_xml.Descendants("Disability").Single().Value),
                TANF = int.Parse(req_xml.Descendants("TANF").Single().Value),
                Health_Insurance = int.Parse(req_xml.Descendants("HealthIns").Single().Value),
                Health_Insurance_Dt = req_xml.Descendants("InsDate").Single().Value, // manual calls this Insurance_History_Date
                Family_Size = int.Parse(req_xml.Descendants("FamSize").Single().Value),
                Adjusted_Income = int.Parse(req_xml.Descendants("FamIncome").Single().Value),
                Episode_Status = int.Parse(req_xml.Descendants("EpStatus").Single().Value),
                Prior_SAEpisodes = int.Parse(req_xml.Descendants("PriorSAEp").Single().Value),
                Dependent_Children = int.Parse(req_xml.Descendants("ChildrenWCare").Single().Value),
                DSC_Status = int.Parse(req_xml.Descendants("DSCStatus").Single().Value),
                Consumer_ZipCode = req_xml.Descendants("ZipCode").Single().Value,
                
                DSC_Start_Date = req_xml.Descendants("DSCDate").Single().Value,
                Death_Date = req_xml.Descendants("Death_Date").Single().Value // not in sample object
            };

            return ep;
        }


        private Encounter createEncounterObj(XDocument req_xml)
        {
            Encounter enc = new Encounter()
            {
                Service_Date = req_xml.Descendants("Service_Date").Single().Value,
                Location_ID = req_xml.Descendants("Location_ID").Single().Value,
                Procedure_Code = req_xml.Descendants("Procedure_Code").Single().Value,
                // Manual describes modifiers as ints, but they are strings
                Modifier_1 = req_xml.Descendants("Modifier_1").Single().Value,
                Modifier_2 = req_xml.Descendants("Modifier_2").Single().Value,
                Modifier_3 = req_xml.Descendants("Modifier_3").Single().Value,
                Modifier_4 = req_xml.Descendants("Modifier_4").Single().Value,
                Units = int.Parse(req_xml.Descendants("Units").Single().Value),
                Is_SUD_Treatment = int.Parse(req_xml.Descendants("IsSUD").Single().Value), // manual calls this SUD_Treatment_Service
                Internal_Service_ID = req_xml.Descendants("Internal_Service_ID").Single().Value, 
                Service_Location = req_xml.Descendants("").Single().Value  // FY2021 add
            };

            return enc;
        }

       
        private NOMS createNOMSObj(XDocument req_xml)
        {
            NOMS noms = new NOMS()
            {
                NOMS_Date = req_xml.Descendants("NOMSDate").SingleOrDefault().Value,
                Reason_Code = int.Parse(req_xml.Descendants("ReasonCode").SingleOrDefault().Value),
                Education_level = int.Parse(req_xml.Descendants("EduLevel").SingleOrDefault().Value),

                SchoolAttendance = int.Parse(req_xml.Descendants("SchStat").SingleOrDefault().Value),
                Employment = int.Parse(req_xml.Descendants("Employ").SingleOrDefault().Value), 
                Employment_Detail = int.Parse(req_xml.Descendants("EmployDet").SingleOrDefault().Value),
                Roles_Score = int.Parse(req_xml.Descendants("ROLES").SingleOrDefault().Value),
                Living_Arrangement = int.Parse(req_xml.Descendants("LiveArr").SingleOrDefault().Value),
                Primary_Substance = int.Parse(req_xml.Descendants("PrimSubst").SingleOrDefault().Value),
                Primary_Route = int.Parse(req_xml.Descendants("PrimRoute").SingleOrDefault().Value),
                Primary_Frequency = int.Parse(req_xml.Descendants("PrimFreq").SingleOrDefault().Value),
                Primary_Age = int.Parse(req_xml.Descendants("PrimAge").SingleOrDefault().Value),
                Secondary_Substance = int.Parse(req_xml.Descendants("SecSubst").SingleOrDefault().Value),
                Secondary_Route = int.Parse(req_xml.Descendants("SecRoute").SingleOrDefault().Value),
                Secondary_Frequency = int.Parse(req_xml.Descendants("SecFreq").SingleOrDefault().Value), 
                Secondary_Age = int.Parse(req_xml.Descendants("SecAge").SingleOrDefault().Value),
                Tertiary_Substance = int.Parse(req_xml.Descendants("TerSubst").SingleOrDefault().Value),
                Tertiary_Route = int.Parse(req_xml.Descendants("TerRoute").SingleOrDefault().Value), 
                Tertiary_Frequency = int.Parse(req_xml.Descendants("TerFreq").SingleOrDefault().Value),
                Tertiary_Age = int.Parse(req_xml.Descendants("TerAge").SingleOrDefault().Value),
                Criminal_Involvement = int.Parse(req_xml.Descendants("Criminal").SingleOrDefault().Value),
                Social_Support = int.Parse(req_xml.Descendants("SocSupp").SingleOrDefault().Value),
                Needle_Use = int.Parse(req_xml.Descendants("NeedleU").SingleOrDefault().Value), 
                Currently_Pregnant = int.Parse(req_xml.Descendants("Pregnant").SingleOrDefault().Value),
                Consumer_Receiving_MAT = int.Parse(req_xml.Descendants("MAT").SingleOrDefault().Value), 
                Prescribed_Medication = int.Parse(req_xml.Descendants("MedPrescr").SingleOrDefault().Value),
                Is_Prescriber_Organization_Staff = int.Parse(req_xml.Descendants("PrescrByUs").SingleOrDefault().Value)  // , // add comma 2021 
                                            
            };

            return noms;
        }

        private DiagnosisAgreementType createDiagAgreeTypeObj(XDocument req_xml)
        {
            DiagnosisAgreementType diagree = new DiagnosisAgreementType()
            {
                Diagnosis_Date = req_xml.Descendants("DiagDate").Single().Value,
                Primary_Diagnosis1 = req_xml.Descendants("Diag1").Single().Value, // called Primary_Diagnosis in manual
                Diagnosis2 = req_xml.Descendants("Diag2").Single().Value,
                Diagnosis3 = req_xml.Descendants("Diag3").Single().Value,
                Diagnosis4 = req_xml.Descendants("Diag4").Single().Value,
                Diagnosis5 = req_xml.Descendants("Diag5").Single().Value,
                SOGS = int.Parse(req_xml.Descendants("SOGS").Single().Value),
                HC_Diabetes = int.Parse(req_xml.Descendants("Diabetes").Single().Value),
                HC_Cardiovascular = int.Parse(req_xml.Descendants("Cardiovascular").Single().Value),
                HC_Hypertension = int.Parse(req_xml.Descendants("Hypertension").Single().Value),
                HC_Hyperlipidemia = int.Parse(req_xml.Descendants("Hyperlipidemia").Single().Value),
                HC_Cancer = int.Parse(req_xml.Descendants("Cancer").Single().Value),
                // HC_Smoking = int.Parse(req_xml.Descendants("Smoking").Single().Value), // FY2021 remove
                HC_Obesity = int.Parse(req_xml.Descendants("Obesity").Single().Value),
                HC_Asthma = int.Parse(req_xml.Descendants("Asthma").Single().Value),
                HC_COPD = int.Parse(req_xml.Descendants("COPD").Single().Value),
                Agreement_Type = int.Parse(req_xml.Descendants("Agreement").Single().Value),
                Specialized_Treatment = int.Parse(req_xml.Descendants("SpecTreat").Single().Value),
                Agreement_Change = int.Parse(req_xml.Descendants("AgChange").Single().Value)
            };

            return diagree;
        }

        private SUDTreatmentAdmission createSUDTreatmentAdmissionObj(XDocument req_xml)
        {
            SUDTreatmentAdmission sud_ad = new SUDTreatmentAdmission()
            {
                Internal_SUD_Admission_ID = req_xml.Descendants("SUDID").Single().Value,
                Admission_Date = req_xml.Descendants("AdmitDate").Single().Value,
                Service_Setting_Type = int.Parse(req_xml.Descendants("ServSetType").Single().Value),
                Location_ID = req_xml.Descendants("Location_ID").Single().Value,
                Primary_Diagnosis = req_xml.Descendants("Diag1").Single().Value,
                Diagnosis_2 = req_xml.Descendants("Diag2").Single().Value,
                Diagnosis_3 = req_xml.Descendants("Diag3").Single().Value,
                Diagnosis_4 = req_xml.Descendants("Diag4").Single().Value,
                Diagnosis_5 = req_xml.Descendants("Diag5").Single().Value,
                SOGS = int.Parse(req_xml.Descendants("SOGS").Single().Value),
                HC_Diabetes = int.Parse(req_xml.Descendants("Diabetes").Single().Value),
                HC_Cardiovascular = int.Parse(req_xml.Descendants("Cardiovascular").Single().Value),
                HC_Hypertension = int.Parse(req_xml.Descendants("Hypertension").Single().Value),
                HC_Hyperlipidemia = int.Parse(req_xml.Descendants("Hyperlipidemia").Single().Value),
                HC_Cancer = int.Parse(req_xml.Descendants("Cancer").Single().Value),
                //HC_Smoking = int.Parse(req_xml.Descendants("Smoking").Single().Value), // FY2021 remove
                HC_Obesity = int.Parse(req_xml.Descendants("Obesity").Single().Value),
                HC_Asthma = int.Parse(req_xml.Descendants("Asthma").Single().Value),
                HC_COPD = int.Parse(req_xml.Descendants("COPD").Single().Value),
                HC_Hepatitis_A = int.Parse(req_xml.Descendants("HepatitisA").Single().Value),
                HC_Hepatitis_B = int.Parse(req_xml.Descendants("HepatitisB").Single().Value),
                HC_Hepatitis_C = int.Parse(req_xml.Descendants("HepatitisC").Single().Value),
                HC_Tuberculosis = int.Parse(req_xml.Descendants("Tuberculosis").Single().Value),
                HC_HIV = int.Parse(req_xml.Descendants("HIV").Single().Value),
                Specialized_Treatment = int.Parse(req_xml.Descendants("SpecTreat").Single().Value),
                Education_level = int.Parse(req_xml.Descendants("EduLevel").Single().Value),
                SchoolAttendance = int.Parse(req_xml.Descendants("SchStat").SingleOrDefault().Value),
                Employment = int.Parse(req_xml.Descendants("Employ").SingleOrDefault().Value),
                Employment_Detail = int.Parse(req_xml.Descendants("EmployDet").SingleOrDefault().Value),
                Roles_Score = int.Parse(req_xml.Descendants("ROLES").SingleOrDefault().Value),
                Living_Arrangement = int.Parse(req_xml.Descendants("LiveArr").SingleOrDefault().Value),
                Primary_Substance = int.Parse(req_xml.Descendants("PrimSubst").SingleOrDefault().Value),
                Primary_Route = int.Parse(req_xml.Descendants("PrimRoute").SingleOrDefault().Value),
                Primary_Frequency = int.Parse(req_xml.Descendants("PrimFreq").SingleOrDefault().Value),
                Primary_Age = int.Parse(req_xml.Descendants("PrimAge").SingleOrDefault().Value),
                Secondary_Substance = int.Parse(req_xml.Descendants("SecSubst").SingleOrDefault().Value),
                Secondary_Route = int.Parse(req_xml.Descendants("SecRoute").SingleOrDefault().Value),
                Secondary_Frequency = int.Parse(req_xml.Descendants("SecFreq").SingleOrDefault().Value),
                Secondary_Age = int.Parse(req_xml.Descendants("SecAge").SingleOrDefault().Value),
                Tertiary_Substance = int.Parse(req_xml.Descendants("TerSubst").SingleOrDefault().Value),
                Tertiary_Route = int.Parse(req_xml.Descendants("TerRoute").SingleOrDefault().Value),
                Tertiary_Frequency = int.Parse(req_xml.Descendants("TerFreq").SingleOrDefault().Value),
                Tertiary_Age = int.Parse(req_xml.Descendants("TerAge").SingleOrDefault().Value),
                Criminal_Involvement = int.Parse(req_xml.Descendants("ArrestsPast30Days").SingleOrDefault().Value),
                Social_Support = int.Parse(req_xml.Descendants("SocSupp").SingleOrDefault().Value),
                Needle_Use = int.Parse(req_xml.Descendants("NeedleU").SingleOrDefault().Value),
                Currently_Pregnant = int.Parse(req_xml.Descendants("Pregnant").SingleOrDefault().Value),
                Consumer_Receiving_MAT = int.Parse(req_xml.Descendants("MAT").SingleOrDefault().Value),
                Prescribed_Medication = int.Parse(req_xml.Descendants("MedPrescr").SingleOrDefault().Value),
                Is_Prescriber_Organization_Staff = int.Parse(req_xml.Descendants("PrescrByUs").SingleOrDefault().Value)  //, // add comma 2021 
            };

            return sud_ad;
        }

        private SUDTreatmentDischarge createSUDTreatmentDischargeObj(XDocument req_xml)
        {
            SUDTreatmentDischarge sud_dx = new SUDTreatmentDischarge()
            {
                Internal_SUD_Admission_ID = req_xml.Descendants("SUDID").Single().Value,
                Discharge_Date = req_xml.Descendants("DischDate").Single().Value,

                Discharge_Reason = int.Parse(req_xml.Descendants("DischReason").Single().Value),
                Employment = int.Parse(req_xml.Descendants("Employ").SingleOrDefault().Value),
                Employment_Detail = int.Parse(req_xml.Descendants("EmployDet").SingleOrDefault().Value),
                Roles_Score = int.Parse(req_xml.Descendants("ROLES").SingleOrDefault().Value),
                Living_Arrangement = int.Parse(req_xml.Descendants("LiveArr").SingleOrDefault().Value),
                Primary_Substance = int.Parse(req_xml.Descendants("PrimSubst").SingleOrDefault().Value),
                Primary_Frequency = int.Parse(req_xml.Descendants("PrimFreq").SingleOrDefault().Value),
                Secondary_Substance = int.Parse(req_xml.Descendants("SecSubst").SingleOrDefault().Value),
                Secondary_Frequency = int.Parse(req_xml.Descendants("SecFreq").SingleOrDefault().Value),
                Tertiary_Substance = int.Parse(req_xml.Descendants("TerSubst").SingleOrDefault().Value),
                Tertiary_Frequency = int.Parse(req_xml.Descendants("TerFreq").SingleOrDefault().Value),
                Criminal_Involvement = int.Parse(req_xml.Descendants("ArrestsPast30Days").SingleOrDefault().Value),
                Social_Support = int.Parse(req_xml.Descendants("SocSupp").SingleOrDefault().Value)  // , // add comma 2021 
            };

            Console.WriteLine("SUD Dx object created with disch reason: " + sud_dx.Discharge_Reason.ToString());

            return sud_dx;
        }



        /********************************************************************
         * Object Dumps
         *******************************************************************/
        private static void ConsumerDump(Consumer client)
        {
            Console.WriteLine("Birth_Date            :  " + client.Birth_Date);
            Console.WriteLine("Sex_Birth             :  " + client.Sex_Birth);  // FY2021 renamed Birth_Gender to Sex_Birth change PHP?
            Console.WriteLine("Gender_Identity       :  " + client.Gender_Identity); // FY2021 renamed Consumer_Gender to Gender_Identity change PHP?
            Console.WriteLine("Gender_Self_Describe  :  " + client.Gender_Self_Describe);  // FY2021 add
            Console.WriteLine("LGBTQ_Member          :  " + client.LGBTQ_Member);  // FY2021 add
            Console.Write("LGBTQ_Consumer_Id...  :  ");
            for (int i = 0; i < client.LGBTQ_Consumer_Identity.Length; i++)
            {
                Console.Write(client.LGBTQ_Consumer_Identity[i] + " ");
            }
            Console.Write("\n");
            Console.WriteLine("LGBTQ_Self_Describe   :  " + client.LGBTQ_Self_Describe);  // FY2021 add

            Console.WriteLine("English_Fluency       :  " + client.English_Fluency);
            Console.WriteLine("Ethnicity             :  " + client.Ethnicity);
            Console.WriteLine("First_Name            :  " + client.First_Name);
            Console.WriteLine("Middle_Name           :  " + client.Middle_Name);
            Console.WriteLine("Mother_Maiden_Name    :  " + client.Mother_Maiden_Name);
            Console.WriteLine("Internal_ID           :  " + client.Internal_ID);
            Console.WriteLine("Last_Name             :  " + client.Last_Name);
            Console.WriteLine("Medicaid_ID           :  " + client.Medicaid_ID);
            Console.WriteLine("Primary_Language      :  " + client.Primary_Language);
            Console.WriteLine("Race_African_American :  " + client.Race_African_American);
            Console.WriteLine("Race_American_Indian  :  " + client.Race_American_Indian);
            Console.WriteLine("Race_Asian            :  " + client.Race_Asian);
            Console.WriteLine("Race_Caucasian        :  " + client.Race_Caucasian);
            Console.WriteLine("Race_Hawaiian         :  " + client.Race_Hawaiian);
            Console.WriteLine("Race_Other            :  " + client.Race_Other);
            Console.WriteLine("SSN                   :  " + client.SSN);
            Console.WriteLine("Suffix_ID             :  " + client.Suffix_ID);
            Console.WriteLine("DOC_ID                :  " + client.DOC_ID);
            Console.WriteLine("\n");
        } // end client_dump

        private static void ConsumerItemDump(ConsumerItem client)
        {
            Console.WriteLine("Birth_Date            :  " + client.Birth_Date);
            Console.WriteLine("Birth_Gender          :  " + client.Sex_Birth); // FY2021 renamed Birth_Gender to Sex_Birth change PHP??
            Console.WriteLine("Consumer_Gender       :  " + client.Gender_Identity);  // FY2021 renamed Consumer_Gender to Gender_Identity change PHP??
            Console.WriteLine("Gender_Self_Describe  :  " + client.Gender_Self_Describe);  // FY2021 add
            Console.WriteLine("LGBTQ_Member          :  " + client.LGBTQ_Member);  // FY2021 add
            Console.Write(    "LGBTQ_Consumer_Id...  :  ");
            if (client.LGBTQ_Consumer_Identity == null)
                Console.Write("GetConsumer returned NULL");
            else
                Array.ForEach(client.LGBTQ_Consumer_Identity, Console.WriteLine);

            Console.Write("\n");
            Console.WriteLine("LGBTQ_Self_Describe   :  " + client.LGBTQ_Self_Describe);  // FY2021 add
            Console.WriteLine("English_Fluency       :  " + client.English_Fluency);
            Console.WriteLine("Ethnicity             :  " + client.Ethnicity);
            Console.WriteLine("First_Name            :  " + client.First_Name);
            Console.WriteLine("Middle_Name           :  " + client.Middle_Name);
            Console.WriteLine("Mother_Maiden_Name    :  " + client.Mother_Maiden_Name);
            Console.WriteLine("Internal_ID           :  " + client.Internal_ID);
            Console.WriteLine("Last_Name             :  " + client.Last_Name);
            Console.WriteLine("Medicaid_ID           :  " + client.Medicaid_ID);
            Console.WriteLine("Primary_Language      :  " + client.Primary_Language);
            Console.WriteLine("Race_African_American :  " + client.Race_African_American);
            Console.WriteLine("Race_American_Indian  :  " + client.Race_American_Indian);
            Console.WriteLine("Race_Asian            :  " + client.Race_Asian);
            Console.WriteLine("Race_Caucasian        :  " + client.Race_Caucasian);
            Console.WriteLine("Race_Hawaiian         :  " + client.Race_Hawaiian);
            Console.WriteLine("Race_Other            :  " + client.Race_Other);
            Console.WriteLine("SSN                   :  " + client.SSN);
            Console.WriteLine("Suffix_ID             :  " + client.Suffix_ID);
            Console.WriteLine("DOC_ID                :  " + client.DOC_ID);
            Console.WriteLine("Error_Code            :  " + client.Error_Code);
            Console.WriteLine("\n");
        } // end client_dump

        // /* not tested 
        private static void AssessHead1Dump(AssessmentHeader1 assess)
        {
            Console.WriteLine("Clinician_ID                 :  " + assess.Clinician_ID);
            Console.WriteLine("Assessment_Date              :  " + assess.Assessment_Date);
            Console.WriteLine("Assessment_Reason_Code       :  " + assess.Assessment_Reason_Code);
            Console.WriteLine("Internal_Episode_Code        :  " + assess.Internal_Episode_Code);
            Console.WriteLine("Internal_Assessment_ID       :  " + assess.Internal_Assessment_ID);
            Console.WriteLine("Strengthening_Families       :  " + assess.Strengthening_Families);
            Console.WriteLine("Parent_Child_Interactive     :  " + assess.Parent_Child_Interactive);
            Console.WriteLine("Child_Parent_Psychotherapy   :  " + assess.Child_Parent_Psychotherapy);
            Console.WriteLine("Incredible_Years             :  " + assess.Incredible_Years);
            Console.WriteLine("Healthy_Families_America     :  " + assess.Healthy_Families_America);  // FY2021 add
            Console.WriteLine("Reporting_Field1             :  " + assess.Reporting_Field1);
            Console.WriteLine("Reporting_Field2             :  " + assess.Reporting_Field2);
            Console.WriteLine("Answer_ID : ");

            for (int i = 0; i < assess.Answer_ID.Length; i++)
            {
                Console.Write(assess.Answer_ID[i] + " ");
            }

            Console.WriteLine(" ");

        } // end cans0-5 dump
        // */

        private static void AssessHead2Dump(AssessmentHeader2 assess)
        {
            Console.WriteLine("Clinician_ID                    :  " + assess.Clinician_ID);
            Console.WriteLine("Assessment_Date                 :  " + assess.Assessment_Date);
            Console.WriteLine("Assessment_Reason_Code          :  " + assess.Assessment_Reason_Code);
            Console.WriteLine("Internal_Episode_Code           :  " + assess.Internal_Episode_Code);
            Console.WriteLine("Internal_Assessment_ID          :  " + assess.Internal_Assessment_ID);
            Console.WriteLine("Trauma_Focused                  :  " + assess.Trauma_Focused);
            Console.WriteLine("Aggression_Replacement          :  " + assess.Aggression_Replacement);
            Console.WriteLine("Cannabis_Youth                  :  " + assess.Cannabis_Youth);
            Console.WriteLine("Strengthening_Families          :  " + assess.Strengthening_Families);
            Console.WriteLine("Parent_Child_Interactive        :  " + assess.Parent_Child_Interactive);
            Console.WriteLine("Structure_Psychotherapy         :  " + assess.Structure_Psychotherapy);
            Console.WriteLine("Dialectical_Behavior            :  " + assess.Dialectical_Behavior);
            Console.WriteLine("Cognitive_Behavior              :  " + assess.Cognitive_Behavior);
            Console.WriteLine("Incredible_Years                :  " + assess.Incredible_Years); 
            Console.WriteLine("Functional_Family               :  " + assess.Functional_Family);
            Console.WriteLine("Alternative_Families            :  " + assess.Alternative_Families);
            Console.WriteLine("Motivational_Interviewing       :  " + assess.Motivational_Interviewing);
            Console.WriteLine("Cognitive_Behavior_Therapy      :  " + assess.Cognitive_Behavior_Therapy);
            Console.WriteLine("Multisystemic_Therapy           :  " + assess.Multisystemic_Therapy);
            Console.WriteLine("Transition_Independence_Process :  " + assess.Transition_Independence_Process);
            Console.WriteLine("CMHI                            :  " + assess.CMHI);  // FY2021 add  
            Console.WriteLine("Reporting_Field1                :  " + assess.Reporting_Field1);
            Console.WriteLine("Reporting_Field2                :  " + assess.Reporting_Field2);
            Console.WriteLine("Answer_ID : ");

            for (int i = 0; i < assess.Answer_ID.Length; i++)
            {
                Console.Write(assess.Answer_ID[i] + " " );
            }

            Console.WriteLine(" ");

        } // end cans5-17 dump

       
        private static void AssessHead3Dump(AssessmentHeader3 assess)
        {
            Console.WriteLine("Clinician_ID               :  " + assess.Clinician_ID);
            Console.WriteLine("Assessment_Date            :  " + assess.Assessment_Date);
            Console.WriteLine("Assessment_Reason_Code     :  " + assess.Assessment_Reason_Code);
            Console.WriteLine("Internal_Episode_Code      :  " + assess.Internal_Episode_Code);
            Console.WriteLine("Internal_Assessment_ID     :  " + assess.Internal_Assessment_ID);
            Console.WriteLine("ACT_Indicator              :  " + assess.ACT_Indicator);
            Console.WriteLine("Aggression_Replacement     :  " + assess.Illness_Management);
            Console.WriteLine("Cannabis_Youth             :  " + assess.Integrated_Dual_Diagnosis);
            Console.WriteLine("Motivational_Interviewing  :  " + assess.Motivational_Interviewing);
            Console.WriteLine("Cognitive_Behavior         :  " + assess.Cognitive_Behavior);
            Console.WriteLine("Matrix_Model               :  " + assess.Matrix_Model);
            Console.WriteLine("Dialectical_Behavior       :  " + assess.Dialectical_Behavior);
            Console.WriteLine("Clubhouse                  :  " + assess.Clubhouse);
            Console.WriteLine("Peer_Support               :  " + assess.Peer_Support);
            Console.WriteLine("Family_Psychoeducation     :  " + assess.Family_Psychoeducation);
            Console.WriteLine("Medication_Management      :  " + assess.Medication_Management);
            Console.WriteLine("Supported_Employment       :  " + assess.Supported_Employment);
            Console.WriteLine("Supported_Housing          :  " + assess.Supported_Housing);
            Console.WriteLine("Reporting_Field1           :  " + assess.Reporting_Field1);
            Console.WriteLine("Reporting_Field2           :  " + assess.Reporting_Field2);
            Console.WriteLine("Answer_ID : ");

            for (int i = 0; i < assess.Answer_ID.Length; i++)
            {
                Console.Write(assess.Answer_ID[i] + " ");
            }

            Console.WriteLine(" ");

        } // end ansa dump


        private static void EncounterDump(Encounter enc)
        {
            Console.WriteLine("Service_Date                  : " + enc.Service_Date);
            Console.WriteLine("Location_ID                   : " + enc.Location_ID);
            Console.WriteLine("Procedure_Code                : " + enc.Procedure_Code);
            Console.WriteLine("Modifier_1                    : " + enc.Modifier_1);
            Console.WriteLine("Modifier_2                    : " + enc.Modifier_2);
            Console.WriteLine("Modifier_3                    : " + enc.Modifier_3);
            Console.WriteLine("Modifier_4                    : " + enc.Modifier_4);
            Console.WriteLine("Units                         : " + enc.Units);
            Console.WriteLine("Is_SUD_Treatment              : " + enc.Is_SUD_Treatment);
            Console.WriteLine("Internal_Service_ID           : " + enc.Internal_Service_ID);
            Console.WriteLine("Service_Location              : " + enc.Service_Location);  // FY2021 add
        }

        private static void EpisodeDump(Episode ep)
        {
            Console.WriteLine("Internal_Episode_Code         : " + ep.Internal_Episode_Code );
            Console.WriteLine("Start_Date                    : " + ep.Start_Date            );
            Console.WriteLine("End_Date                      : " + ep.End_Date              );
            Console.WriteLine("Medicaid_Active               : " + ep.Medicaid_Active       );
            Console.WriteLine("Marital_Status                : " + ep.Marital_Status        );
            Console.WriteLine("County                        : " + ep.County                );
            Console.WriteLine("FoodStamps                    : " + ep.FoodStamps            );
            Console.WriteLine("Referral_Source               : " + ep.Referral_Source       );
            Console.WriteLine("Legal_Basis                   : " + ep.Legal_Basis           );
            Console.WriteLine("Military_Served               : " + ep.Military_Served       );
            Console.WriteLine("Military_Veteran              : " + ep.Military_Veteran      );
            Console.WriteLine("Military_Deployed             : " + ep.Military_Deployed     );
            Console.WriteLine("Military_Combat               : " + ep.Military_Combat       );
            Console.WriteLine("Military_Family               : " + ep.Military_Family       );
            Console.WriteLine("Consumer_Disability           : " + ep.Consumer_Disability   );
            Console.WriteLine("TANF                          : " + ep.TANF                  );
            Console.WriteLine("Health_Insurance              : " + ep.Health_Insurance      );
            Console.WriteLine("Health_Insurance_Dt           : " + ep.Health_Insurance_Dt   );
            Console.WriteLine("Family_Size                   : " + ep.Family_Size           );
            Console.WriteLine("Adjusted_Income               : " + ep.Adjusted_Income       );
            Console.WriteLine("Episode_Status                : " + ep.Episode_Status        );
            Console.WriteLine("Prior_SAEpisodes              : " + ep.Prior_SAEpisodes      );
            Console.WriteLine("Dependent_Children            : " + ep.Dependent_Children    );
            Console.WriteLine("DSC_Status                    : " + ep.DSC_Status            );
            Console.WriteLine("Consumer_ZipCode              : " + ep.Consumer_ZipCode      );
            Console.WriteLine("DSC_Start_Date                : " + ep.DSC_Start_Date        );
            Console.WriteLine("Death_Date                    : " + ep.Death_Date            );

        }

        private static void SubstanceListDump(List<SubstanceUse> subslist)
        {
            Console.WriteLine("\n#### SubstanceUseList Dump : Count = {0} ####", subslist.Count);

            if (subslist.Count == 0) { Console.WriteLine("No substances identified in this assessment.\n\n"); }

            foreach (var subs in subslist)
            {
                Console.WriteLine("Substance                     : " + subs.Substance);

                if (subs.Currently_Used != null)
                { Console.WriteLine("Currently_Used                : " + subs.Currently_Used); }

                if (subs.Outpatient != null)
                { Console.WriteLine("Outpatient                    : " + subs.Outpatient); }

                if (subs.Helpful_Outpatient != null)
                { Console.WriteLine("Helpful_Outpatient            : " + subs.Helpful_Outpatient); }

                if (subs.Intensive != null)
                { Console.WriteLine("Intensive                     : " + subs.Intensive); }

                if (subs.Helpful_Intensive != null)
                { Console.WriteLine("Helpful_Intensive             : " + subs.Helpful_Intensive); }

                if (subs.Partial_Hospitalization != null)
                { Console.WriteLine("Partial_Hospitalization       : " + subs.Partial_Hospitalization); }

                if (subs.Helpful_Partial_Hospitalization != null)
                { Console.WriteLine("Helpful_Partial_Hosp          : " + subs.Helpful_Partial_Hospitalization); }

                if (subs.MAT != null)
                { Console.WriteLine("MAT                           : " + subs.MAT); }

                if (subs.Helpful_MAT != null)
                { Console.WriteLine("Helpful_MAT                   : " + subs.Helpful_MAT); }

                if (subs.Residential != null)
                { Console.WriteLine("Residential                   : " + subs.Residential); }

                if (subs.Helpful_Residential != null)
                { Console.WriteLine("Helpful_Residential           : " + subs.Helpful_Residential); }

                if (subs.Inpatient != null)
                { Console.WriteLine("Inpatient                     : " + subs.Inpatient); }

                if (subs.Helpful_Inpatient != null)
                { Console.WriteLine("Helpful_Inpatient             : " + subs.Helpful_Inpatient); }

                if (subs.Route != null)
                { Console.WriteLine("Route                         : " + subs.Route); }

                if (subs.Frequency != null)
                { Console.WriteLine("Frequency                     : " + subs.Frequency); }

                if (subs.Twenty_FourHour_Use != null)
                { Console.WriteLine("Twenty_FourHour_Use           : " + subs.Twenty_FourHour_Use); }
                
                Console.WriteLine("\n");
            }
        }


        /********************************************************************
         * Helper Functions
         *******************************************************************/
        private string ClinCertToXMLStr(ClinicianCertificationItem[] clin_certs)
        {
            XDocument ClinCertsXML = new XDocument(
                new XDeclaration("1.0", "utf-8", "yes"),
                new XElement("ClinCerts"));

            foreach (ClinicianCertificationItem clincert in clin_certs)
            {
                ClinCertsXML.Root.Add(
                    new XElement("clincert",
                        new XElement("Certification_Type_ID", clincert.Certification_Type_ID.ToString()),
                        new XElement("Certification_Desc", clincert.Certification_Desc),
                        new XElement("Effective_Start_Date", clincert.Effective_Start_Date),
                        new XElement("Expiration_Date", clincert.Expiration_Date),
                        new XElement("Error_Code", clincert.Error_Code)
                        ));
            }

            return ClinCertsXML.ToString();
        }

        private string SubsItemsToXMLStr(SubstanceItem[] subs_items)
        {
            XDocument SubsItemsXML = new XDocument(
                new XDeclaration("1.0", "utf-8", "yes"),
                new XElement("SubsCodes"));

            foreach (SubstanceItem subsitem in subs_items)
            {
                SubsItemsXML.Root.Add(
                    new XElement("SubsCode",
                        // note property Substance_ID is misspelled as Susbtance_ID
                        new XElement("Substance_ID", subsitem.Susbtance_ID.ToString()),
                        new XElement("Substance_Desc", subsitem.Substance_Desc),
                        new XElement("Error_Code", subsitem.Error_Code)
                        ));
            }

            return SubsItemsXML.ToString();
        }

        private string DiagItemsToXMLStr(DiagnosisCodeItem[] diag_items)
        {
            XDocument DiagItemsXML = new XDocument(
                new XDeclaration("1.0", "utf-8", "yes"),
                new XElement("DiagCodes"));

            foreach (DiagnosisCodeItem diagitem in diag_items)
            {
                DiagItemsXML.Root.Add(
                    new XElement("DiagCode",
                        new XElement("DiagnosisID",    diagitem.Diagnosis_ID.ToString()),
                        new XElement("DiagnosisCode",  diagitem.Diagnosis_Code),
                        new XElement("DiagnosisDesc",  diagitem.Diagnosis_Desc),
                        new XElement("LeadingCause",   diagitem.Leading_Cause_Ind.ToString()),
                        new XElement("Error_Code",     diagitem.Error_Code)
                        ));
            }

            return DiagItemsXML.ToString();
        }

        private string AssessResultItemsToXMLStr(AssessmentResultItem[] assess_results)
        {
            XDocument AssessItemsXML = new XDocument(
                new XDeclaration("1.0", "utf-8", "yes"),
                new XElement("AssessResults"));

            foreach (AssessmentResultItem assessresult in assess_results)
            {
                AssessItemsXML.Root.Add(
                    new XElement("AssessResult",
                        new XElement("Visit_ID", assessresult.Visit_ID.ToString()), 
                        new XElement("Algorithm_Desc", assessresult.Algorithm_Desc), 
                        new XElement("Option_Desc_Short", assessresult.Option_Desc_Short), 
                        new XElement("Option_Desc_Long",  assessresult.Option_Desc_Long),
                        new XElement("Option_Level", assessresult.Option_Level.ToString()),
                        new XElement("Error_Code", assessresult.Error_Code)
                        ));
            }

            return AssessItemsXML.ToString();
        }

        private string EpisodeItemListToXMLStr(EpisodeItem[] ep_items)
        {
            XDocument EpItemsXML = new XDocument(
                new XDeclaration("1.0", "utf-8", "yes"),
                new XElement("Episodes"));

            foreach (EpisodeItem ep_item in ep_items)
            {
                EpItemsXML.Root.Add(
                    new XElement("EpisodeItem",
                        new XElement("Internal_Episode_Code", ep_item.Internal_Episode_Code),
                        new XElement("Start_Date", ep_item.Start_Date),
                        new XElement("End_Date", ep_item.End_Date),
                        new XElement("Medicaid_Active", ep_item.Medicaid_Active),
                        new XElement("Marital_Status", ep_item.Marital_Status),
                        new XElement("County", ep_item.County),
                        new XElement("FoodStamps", ep_item.FoodStamps),
                        new XElement("Referral_Source", ep_item.Referral_Source),
                        new XElement("Legal_Basis", ep_item.Legal_Basis),
                        new XElement("Military_Served", ep_item.Military_Served),
                        new XElement("Military_Veteran", ep_item.Military_Veteran),
                        new XElement("Military_Deployed", ep_item.Military_Deployed),
                        new XElement("Military_Combat", ep_item.Military_Combat),
                        new XElement("Military_Family", ep_item.Military_Family),
                        new XElement("Consumer_Disability", ep_item.Consumer_Disability),
                        new XElement("TANF", ep_item.TANF),
                        new XElement("Health_Insurance", ep_item.Health_Insurance),
                        new XElement("Health_Insurance_Dt", ep_item.Health_Insurance_Dt),
                        new XElement("Family_Size", ep_item.Family_Size),
                        new XElement("Adjusted_Income", ep_item.Adjusted_Income),
                        new XElement("Episode_Status", ep_item.Episode_Status),
                        new XElement("Prior_SAEpisodes", ep_item.Prior_SAEpisodes),
                        new XElement("Dependent_Children", ep_item.Dependent_Children),
                        new XElement("DSC_Status", ep_item.DSC_Status),
                        new XElement("ZipCode", ep_item.Consumer_ZipCode),
                        new XElement("DSC_Start_Date", ep_item.DSC_Status),
                        new XElement("Death_Date", ep_item.Death_Date),
                        new XElement("Error_Code", ep_item.Error_Code)
                    ));
            }

            return EpItemsXML.ToString();

        }

        private string EpisodeItemToXMLStr(EpisodeItem ep)
        {   
            XDocument EpisodeItemXML = new XDocument(
                new XDeclaration("1.0", "utf-8", "yes"),
                new XElement("EpisodeItem", 
                    new XElement("Internal_Episode_Code", ep.Internal_Episode_Code), 
                    new XElement("Start_Date", ep.Start_Date), 
                    new XElement("End_Date", ep.End_Date),
                    new XElement("Medicaid_Active", ep.Medicaid_Active),
                    new XElement("Marital_Status", ep.Marital_Status),
                    new XElement("County", ep.County),
                    new XElement("FoodStamps", ep.FoodStamps),
                    new XElement("Referral_Source", ep.Referral_Source),
                    new XElement("Legal_Basis", ep.Legal_Basis),
                    new XElement("Military_Served", ep.Military_Served),
                    new XElement("Military_Veteran", ep.Military_Veteran),
                    new XElement("Military_Deployed", ep.Military_Deployed),
                    new XElement("Military_Combat", ep.Military_Combat),
                    new XElement("Military_Family", ep.Military_Family),
                    new XElement("Consumer_Disability", ep.Consumer_Disability),
                    new XElement("TANF", ep.TANF),
                    new XElement("Health_Insurance", ep.Health_Insurance),
                    new XElement("Health_Insurance_Dt", ep.Health_Insurance_Dt),
                    new XElement("Family_Size", ep.Family_Size),
                    new XElement("Adjusted_Income", ep.Adjusted_Income),
                    new XElement("Episode_Status", ep.Episode_Status), 
                    new XElement("Prior_SAEpisodes", ep.Prior_SAEpisodes),
                    new XElement("Dependent_Children", ep.Dependent_Children), 
                    new XElement("DSC_Status", ep.DSC_Status), 
                    new XElement("ZipCode", ep.Consumer_ZipCode),
                    new XElement("DSC_Start_Date", ep.DSC_Start_Date), 
                    new XElement("Death_Date", ep.Death_Date),
                    new XElement("Error_Code", ep.Error_Code)
                )
                );

            return EpisodeItemXML.ToString();
        }

        private string EncounterItemToXMLStr(EncounterItem enc)
        {
            XDocument EncounterItemXML = new XDocument(
                new XDeclaration("1.0", "utf-8", "yes"),
                new XElement("Encounter", 
                    new XElement("Service_Date", enc.Service_Date),
                    new XElement("Location_ID", enc.Location_ID),
                    new XElement("Procedure_Code", enc.Procedure_Code),
                    new XElement("Modifier_1", enc.Modifier_1),
                    new XElement("Modifier_2", enc.Modifier_2),
                    new XElement("Modifier_3", enc.Modifier_3),
                    new XElement("Modifier_4", enc.Modifier_4),
                    new XElement("Units", enc.Units),
                    new XElement("IsSUD", enc.Is_SUD_Treatment), 
                    new XElement("Internal_Service_ID", enc.Internal_Service_ID),
                    new XElement("", enc.Service_Location)  // FY2021 add
                    )
                    );

            return EncounterItemXML.ToString();
        }

        private string ConsToXMLStr(ConsumerItem cons_obj)
        {
            XDocument ConsXML = new XDocument(
                new XDeclaration("1.0", "utf-8", "yes"),
                new XElement("Consumer",
                    new XElement("BirthDate", cons_obj.Birth_Date),
                    new XElement("BirthGender", cons_obj.Sex_Birth),  // FY2021 rename to Sex_Birth    change PHP?  
                    new XElement("Gender", cons_obj.Gender_Identity),  // FY2021 renamed Consumer_Gender to Gender_Identity  change PHP?
                    new XElement("Gender_Self_Descrxibe", cons_obj.Gender_Self_Describe),  // FY2021 add
                    new XElement("LGBTQ_Member", cons_obj.LGBTQ_Member),  // FY2021 add
                    new XElement("LGBTQ_Consumer_Identity", cons_obj.LGBTQ_Consumer_Identity),  // FY2021 add - ACCOUNT FOR ARRAY!!!!!
                    new XElement("LGBTQ_Self_Describe", cons_obj.LGBTQ_Self_Describe),  // FY2021 add

                    new XElement("English", cons_obj.English_Fluency),
                    new XElement("Ethnicity", cons_obj.Ethnicity),
                    new XElement("FirstName", cons_obj.First_Name),
                    new XElement("MiddleName", cons_obj.Middle_Name),
                    new XElement("Internal_ID", cons_obj.Internal_ID),
                    new XElement("LastName", cons_obj.Last_Name),
                    new XElement("MomMaiden", cons_obj.Mother_Maiden_Name),
                    new XElement("MedicaidID", cons_obj.Medicaid_ID),
                    new XElement("PrimLang", cons_obj.Primary_Language),
                    new XElement("Black", cons_obj.Race_African_American),
                    new XElement("NativeAm", cons_obj.Race_American_Indian),
                    new XElement("Asian", cons_obj.Race_Asian),
                    new XElement("White", cons_obj.Race_Caucasian),
                    new XElement("Hawaiian", cons_obj.Race_Hawaiian),
                    new XElement("OtherRace", cons_obj.Race_Other),
                    new XElement("SSN", cons_obj.SSN),
                    new XElement("CorrectionsID", cons_obj.DOC_ID),
                    new XElement("SuffixID", cons_obj.Suffix_ID), 
                    new XElement("Error_Code", cons_obj.Error_Code)
                )
                );
            return ConsXML.ToString();
        }

        public void LogMessage(string msg)
        {
            using (StreamWriter logwriter = File.AppendText(logfile))
            {
                logwriter.WriteLine(msg);
                logwriter.WriteLine("\n");
            }
        }

        public void LogException(Exception ex)
        {
            using (StreamWriter logwriter = File.AppendText(logfile))
            {
                logwriter.WriteLine(DateTime.Now);
                logwriter.WriteLine("ERROR: " + ex.GetType().Name);
                logwriter.WriteLine(ex.Message);
                logwriter.WriteLine(ex.StackTrace);
                logwriter.WriteLine("\n");
            }
        }

        public void LogException(Exception ex, XDocument req)
        {
            using (StreamWriter logwriter = File.AppendText(logfile))
            {
                string request = req.ToString();
                logwriter.WriteLine(DateTime.Now);
                logwriter.WriteLine("XML Request: " + request + "\n");
                logwriter.WriteLine("ERROR: " + ex.GetType().Name);
                logwriter.WriteLine(ex.Message);
                logwriter.WriteLine(ex.StackTrace);
                logwriter.WriteLine("\n");
            }
        }

        public void LogException(string call, Exception ex, XDocument req)
        {
            using (StreamWriter logwriter = File.AppendText(logfile))
            {
                string request = req.ToString();
                logwriter.WriteLine("*** Function Call: " + call + " ***  " + DateTime.Now);
                logwriter.WriteLine("XML Request: " + request + "\n");
                logwriter.WriteLine("ERROR: " + ex.GetType().Name);
                logwriter.WriteLine(ex.Message);
                logwriter.WriteLine(ex.StackTrace);
                logwriter.WriteLine("\n");
            }
        }

        public void DumpException(Exception ex)
        {
            Console.WriteLine("ERROR: " + ex.GetType().Name);
            Console.WriteLine(ex.Message);
            Console.WriteLine(ex.StackTrace);
        }


        private string questionsToXMLStr(AssessmentToolQuestionItem[] q_array)
        {
            XDocument QuestionsXML = new XDocument(
                new XDeclaration("1.0", "utf-8", "yes"),
                new XElement("Questions"));

            foreach (AssessmentToolQuestionItem question in q_array)
            {
                QuestionsXML.Root.Add(
                    new XElement("question",
                        new XElement("Module_ID", question.Module_ID),
                        new XElement("Module_Name", question.Module_Name),
                        new XElement("Question_ID", question.Question_ID),
                        new XElement("Question_Text", question.Question_Text),
                        new XElement("Question_Clarification", question.Question_Clarification),
                        new XElement("Question_Order", question.Question_Order),
                        new XElement("Answer_ID", question.Answer_Desc),
                        new XElement("Answer_Desc", question.Answer_Desc),
                        new XElement("Answer_Value", question.Answer_Value),
                        new XElement("Error_Code", question.Error_Code)
                        ));
            }

            Console.WriteLine("AssessmentToolQuestions:  " + QuestionsXML.ToString());

            return QuestionsXML.ToString();
        }

        private string ToolsToXMLStr(AssessmentToolItem[] assess_tools)
        {
            XDocument ToolsXML = new XDocument(
                new XDeclaration("1.0", "utf-8", "yes"),
                new XElement("Tools"));

            foreach (AssessmentToolItem tool in assess_tools)
            {
                ToolsXML.Root.Add(
                    new XElement("tool",
                        new XElement("Tool_ID", tool.Tool_ID),
                        new XElement("Tool_Desc_Long", tool.Tool_Desc_Long),
                        new XElement("Tool_Desc_Short", tool.Tool_Desc_Short),
                        new XElement("Group_ID", tool.Group_ID),
                        new XElement("Group_Desc", tool.Group_Desc),
                        new XElement("Error_Code", tool.Error_Code)
                        ));
            }

            return ToolsXML.ToString();
        }
    }
}
