/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Net;
using System.Net.Security;
using System.Security.Cryptography;
using System.Text;

namespace GKMap.MapProviders
{
    /// <summary>
    /// base class for each map provider
    /// </summary>
    public abstract class GMapProvider
    {
        /// <summary>
        /// Time to live of cache, in hours. Default: 240 (10 days).
        /// </summary>
        public const int TTLCache = 240;

        private string fAuthorization = string.Empty;
        private bool fIsInitialized;

        private static string fLanguageStr = "en";
        private static LanguageType fLanguage = LanguageType.English;

        /// <summary>
        /// was provider initialized
        /// </summary>
        public bool IsInitialized
        {
            get {
                return fIsInitialized;
            }
            internal set {
                fIsInitialized = value;
            }
        }

        /// <summary>
        /// unique provider id
        /// </summary>
        public virtual Guid Id
        {
            get {
                throw new NotImplementedException();
            }
        }

        /// <summary>
        /// provider name
        /// </summary>
        public virtual string Name
        {
            get {
                throw new NotImplementedException();
            }
        }

        /// <summary>
        /// provider projection
        /// </summary>
        public virtual PureProjection Projection
        {
            get {
                throw new NotImplementedException();
            }
        }

        /// <summary>
        /// provider overlays
        /// </summary>
        public virtual GMapProvider[] Overlays
        {
            get {
                throw new NotImplementedException();
            }
        }

        /// <summary>
        /// gets tile image using implemented provider
        /// </summary>
        /// <param name="pos"></param>
        /// <param name="zoom"></param>
        /// <returns></returns>
        public virtual PureImage GetTileImage(GPoint pos, int zoom)
        {
            throw new NotImplementedException();
        }

        private static readonly List<GMapProvider> MapProviders = new List<GMapProvider>();

        protected GMapProvider()
        {
            using (var sha1 = SHA1.Create()) {
                DbId = Math.Abs(BitConverter.ToInt32(sha1.ComputeHash(Id.ToByteArray()), 0));
            }

            if (MapProviders.Exists(p => p.Id == Id || p.DbId == DbId)) {
                throw new Exception("such provider id already exists, try regenerate your provider guid...");
            }
            MapProviders.Add(this);
        }

        /// <summary>
        /// id for database, a hash of provider guid
        /// </summary>
        public readonly int DbId;

        /// <summary>
        /// area of map
        /// </summary>
        public RectLatLng? Area;

        /// <summary>
        /// minimum level of zoom
        /// </summary>
        public int MinZoom;

        /// <summary>
        /// maximum level of zoom
        /// </summary>
        public int? MaxZoom = 17;

        /// <summary>
        /// proxy for net access
        /// </summary>
        public static IWebProxy WebProxy;

        /// <summary>
        /// Connect trough a SOCKS 4/5 proxy server
        /// </summary>
        public static bool IsSocksProxy = false;

        /// <summary>
        /// NetworkCredential for tile http access
        /// </summary>
        public static ICredentials Credential;

        /// <summary>
        /// Gets or sets the value of the User-agent HTTP header.
        /// It's pseudo-randomized to avoid blockages...
        /// </summary>
        public static string UserAgent = string.Format("Mozilla/5.0 (Windows NT {1}.0; {2}rv:{0}.0) Gecko/20100101 Firefox/{0}.0",
            Stuff.Random.Next(DateTime.Today.Year - 1969 - 5, DateTime.Today.Year - 1969),
            Stuff.Random.Next(0, 10) % 2 == 0 ? 10 : 6,
            Stuff.Random.Next(0, 10) % 2 == 1 ? string.Empty : "WOW64; ");

        /// <summary>
        /// timeout for provider connections
        /// </summary>
        public static int TimeoutMs = 10 * 1000;

        /// <summary>
        /// Gets or sets the value of the Referer HTTP header.
        /// </summary>
        public string RefererUrl = string.Empty;

        public string Copyright = string.Empty;

        /// <summary>
        /// true if tile origin at BottomLeft, WMS-C
        /// </summary>
        public bool InvertedAxisY = false;

        /// <summary>
        /// map language
        /// </summary>
        public static string LanguageStr
        {
            get { return fLanguageStr; }
            set { fLanguageStr = value; }
        }

        public static LanguageType Language
        {
            get {
                return fLanguage;
            }
            set {
                fLanguage = value;
                fLanguageStr = Stuff.EnumToString(fLanguage);
            }
        }

        private static readonly string RequestAccept = "*/*";
        private static readonly string ResponseContentType = "image";


        static GMapProvider()
        {
            WebProxy = EmptyWebProxy.Instance;
        }

        /// <summary>
        /// called before first use
        /// </summary>
        public virtual void OnInitialized()
        {
            // nice place to detect current provider version
        }

        protected virtual bool CheckTileImageHttpResponse(WebResponse response)
        {
            //Debug.WriteLine(response.StatusCode + "/" + response.StatusDescription + "/" + response.ContentType + " -> " + response.ResponseUri);
            return response.ContentType.Contains(ResponseContentType);
        }

        /// <summary>
        /// http://blog.kowalczyk.info/article/at3/Forcing-basic-http-authentication-for-HttpWebReq.html
        /// </summary>
        /// <param name="userName"></param>
        /// <param name="userPassword"></param>
        public void ForceBasicHttpAuthentication(string userName, string userPassword)
        {
            fAuthorization = "Basic " + Convert.ToBase64String(Encoding.UTF8.GetBytes(userName + ":" + userPassword));
        }

        private WebRequest GetRequest(string url)
        {
            /*
             * Solution for exception on Xamarin:
             * System.Net.WebException: Error: TrustFailure (Authentication failed, see inner exception.)
             * System.Security.Authentication.AuthenticationException: Authentication failed, see inner exception.
             * Mono.Btls.MonoBtlsException: Ssl error:1000007d:SSL routines:OPENSSL_internal:CERTIFICATE_VERIFY_FAILED
             */
            ServicePointManager.ServerCertificateValidationCallback = new RemoteCertificateValidationCallback(
               delegate { return true; }
            );

            WebRequest request = IsSocksProxy ? SocksHttpWebRequest.Create(url) : WebRequest.Create(url);

            if (WebProxy != null) {
                request.Proxy = WebProxy;
            }

            if (Credential != null) {
                request.PreAuthenticate = true;
                request.Credentials = Credential;
            }

            if (!string.IsNullOrEmpty(fAuthorization)) {
                request.Headers.Set("Authorization", fAuthorization);
            }

            if (request is HttpWebRequest) {
                var r = request as HttpWebRequest;
                r.UserAgent = UserAgent;
                r.ReadWriteTimeout = TimeoutMs * 6;
                r.Accept = RequestAccept;
                r.Referer = RefererUrl;
                r.Timeout = TimeoutMs;
            } else if (request is SocksHttpWebRequest) {
                var r = request as SocksHttpWebRequest;

                if (!string.IsNullOrEmpty(UserAgent)) {
                    r.Headers.Add("User-Agent", UserAgent);
                }

                if (!string.IsNullOrEmpty(RequestAccept)) {
                    r.Headers.Add("Accept", RequestAccept);
                }

                if (!string.IsNullOrEmpty(RefererUrl)) {
                    r.Headers.Add("Referer", RefererUrl);
                }
            }

            return request;
        }

        protected PureImage GetTileImageUsingHttp(string url)
        {
            PureImage ret = null;

            WebRequest request = GetRequest(url);
            using (var response = request.GetResponse()) {
                if (CheckTileImageHttpResponse(response)) {
                    using (Stream responseStream = response.GetResponseStream()) {
                        MemoryStream data = Stuff.CopyStream(responseStream, false);

                        Debug.WriteLine("Response[" + data.Length + " bytes]: " + url);

                        if (data.Length > 0) {
                            ret = GMaps.TileImageProxy.FromStream(data);

                            if (ret != null) {
                                ret.Data = data;
                                ret.Data.Position = 0;
                            } else {
                                data.Dispose();
                            }
                        }
                    }
                } else {
                    Debug.WriteLine("CheckTileImageHttpResponse[false]: " + url);
                }
                response.Close();
            }
            return ret;
        }

        protected string GetContentUsingHttp(string url)
        {
            string ret;

            WebRequest request = GetRequest(url);
            using (var response = request.GetResponse()) {
                using (Stream responseStream = response.GetResponseStream()) {
                    using (StreamReader read = new StreamReader(responseStream, Encoding.UTF8)) {
                        ret = read.ReadToEnd();
                    }
                }
                response.Close();
            }

            return ret;
        }

        protected static int GetServerNum(GPoint pos, int max)
        {
            return (int)(pos.X + 2 * pos.Y) % max;
        }

        public override int GetHashCode()
        {
            return DbId;
        }

        public override bool Equals(object obj)
        {
            if (obj is GMapProvider) {
                return Id.Equals(((GMapProvider) obj).Id);
            }
            return false;
        }

        public override string ToString()
        {
            return Name;
        }
    }
}
