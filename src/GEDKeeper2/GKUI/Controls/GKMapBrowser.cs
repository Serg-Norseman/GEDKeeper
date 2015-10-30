using System;
using System.Globalization;
using System.IO;
using System.Net;
using System.Windows.Forms;
using System.Xml;

using GKCommon;
using GKCore.Maps;
using GKCore.Options;

namespace GKUI.Controls
{
	/// <summary>
	/// 
	/// </summary>
	public class GKMapBrowser : WebBrowser
	{
		private struct CoordsRect
		{
			public double MinLon;
			public double MinLat;
			public double MaxLon;
			public double MaxLat;
		}

		private readonly ExtList<GMapPoint> fMapPoints;
		private bool fShowPoints;
		private bool fShowLines;
		private int fUpdateCount;


		public bool ShowPoints
		{
			get {
				return this.fShowPoints;
			}
			set {
				this.fShowPoints = value;
				this.RefreshPoints();
			}
		}

		public bool ShowLines
		{
			get {
				return this.fShowLines;
			}
			set {
				this.fShowLines = value;
				this.RefreshPoints();
			}
		}


		public GMapPoint GetMapPoint(int index)
		{
			GMapPoint result = null;
			if (index >= 0 && index < this.fMapPoints.Count)
			{
				result = (this.fMapPoints[index] as GMapPoint);
			}
			return result;
		}

		public int GetMapPointsCount()
		{
			return this.fMapPoints.Count;
		}


		public GKMapBrowser()
		{
			this.fMapPoints = new ExtList<GMapPoint>(true);
			this.fUpdateCount = 0;
			this.fShowPoints = true;
			this.fShowLines = true;
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.ClearPoints();
				this.fMapPoints.Dispose();
			}
			base.Dispose(disposing);
		}


		private CoordsRect GetPointsFrame()
		{
			CoordsRect result = new CoordsRect();
			if (this.fMapPoints.Count > 0)
			{
				GMapPoint pt = this.fMapPoints[0] as GMapPoint;
				result.MinLon = pt.Longitude;
				result.MaxLon = pt.Longitude;
				result.MinLat = pt.Latitude;
				result.MaxLat = pt.Latitude;

				if (this.fMapPoints.Count == 1)
				{
					result.MinLon = (result.MinLon - 20.0);
					result.MaxLon = (result.MaxLon + 20.0);
					result.MinLat = (result.MinLat - 20.0);
					result.MaxLat = (result.MaxLat + 20.0);
				}
				else
				{
					int num = this.fMapPoints.Count;
					for (int i = 0; i < num; i++)
					{
						pt = (this.fMapPoints[i] as GMapPoint);

						if (result.MinLon > pt.Longitude) result.MinLon = pt.Longitude;
						else if (result.MaxLon < pt.Longitude) result.MaxLon = pt.Longitude;

						if (result.MinLat > pt.Latitude) result.MinLat = pt.Latitude;
						else if (result.MaxLat < pt.Latitude) result.MaxLat = pt.Latitude;
					}
				}
			}
			return result;
		}

		public int AddPoint(double latitude, double longitude, string hint)
		{
			GMapPoint pt = new GMapPoint(latitude, longitude, hint);
			return this.fMapPoints.Add(pt);
		}

		public void ClearPoints()
		{
			this.gm_ClearPoints();
			this.fMapPoints.Clear();
		}

		public void DeletePoint(int index)
		{
			this.fMapPoints.Delete(index);
			this.RefreshPoints();
		}

		public void BeginUpdate()
		{
			this.fUpdateCount++;
		}

		public void EndUpdate()
		{
			this.fUpdateCount--;

			if (this.fUpdateCount <= 0)
			{
				this.RefreshPoints();
				this.fUpdateCount = 0;
			}
		}

		public void InitMap()
		{
			const string MapContent = 
				"<html>" +
				"<head>" +
				"<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\"/>" +
				"<script src=\"http://maps.googleapis.com/maps/api/js?sensor=false&language=ru&key=AIzaSyCo57eNeJx7-ws2eei6QgAVUxOnS95IqQM\" type=\"text/javascript\"></script>" +
				"<script type=\"text/javascript\">" +
				"var map;" +
				"var markersArray = [];" + 
				"function addMarker(latitude, longitude, hint) { " +
				"	var latlng = new google.maps.LatLng(latitude,longitude); " +
				"	var marker = new google.maps.Marker({ position: latlng, map: map, title: hint }); " +
				"	markersArray.push(marker);" +
				"} " +
				"function clearOverlays() {" +
  				"	for (var i = 0; i < markersArray.length; i++ ) {" +
    			"		markersArray[i].setMap(null);" +
  				"	}" +
  				"	markersArray.length = 0;" +
				"}" +
				"function initialize() { " +
				"	var mapOptions = {" +
          		"		center: new google.maps.LatLng(55.755786, 37.617633)," +
          		"		zoom: 8," + 
          		"		mapTypeId: google.maps.MapTypeId.TERRAIN" +
        		"	};" +
				"	map = new google.maps.Map(document.getElementById(\"map\"), mapOptions); " +
				"}" +
				"</script>" +
				"</head>" +
				"<body onload=\"initialize()\">" +
				"<div id=\"map\" style=\"position:absolute; width: 100%; height: 100%; left: 0px; top: 0px;\"></div>" +
				"<noscript>JavaScript must be switched on for use Google Maps.</noscript>" +
				"</body></html>";			
			
			this.DocumentText = MapContent;
		}

		public static string CoordToStr(double val) {
			NumberFormatInfo nfi = new NumberFormatInfo();
			nfi.NumberDecimalSeparator = ".";
			return val.ToString("0.000000", nfi);
		}

		public void RefreshPoints()
		{
			this.gm_ClearPoints();

			if (this.fMapPoints.Count > 0)
			{
				string pointsScript = "";
				string polylineScript = "";

				int num = this.fMapPoints.Count;
				for (int i = 0; i < num; i++)
				{
					GMapPoint pt = this.fMapPoints[i] as GMapPoint;
					pointsScript += string.Format("addMarker({0}, {1}, \"{2}\");", new object[]
					{ CoordToStr(pt.Latitude), CoordToStr(pt.Longitude), pt.Hint });

					polylineScript = string.Concat(new string[]
					{
						polylineScript, "new google.maps.LatLng(",	
						CoordToStr(pt.Latitude), ",", CoordToStr(pt.Longitude), "),"
					});
				}

				if (this.ShowPoints)
				{
					this.gm_ExecScript(pointsScript);
				}

				if (this.ShowLines)
				{
					int num2 = (polylineScript != null) ? polylineScript.Length : 0;
					polylineScript = polylineScript.Remove(num2 - 1, 1);
					polylineScript = 
						"var polyline = new google.maps.Polyline({path: [" + polylineScript + "],strokeColor: \"#FF0000\", strokeWeight: 3}); " +
						"polyline.setMap(map);"+
						"markersArray.push(polyline);";
					this.gm_ExecScript(polylineScript);
				}
			}
		}

		public void SaveSnapshot(string fileName)
		{
		}

		public void SetCenter(double latitude, double longitude, int scale)
		{
			string script;
			if (scale >= 0) {
				script = string.Concat(new string[] {
					"var point = new google.maps.LatLng(", CoordToStr(latitude), ",", CoordToStr(longitude), "); ", 
					"map.setCenter(point)",
					"map.setZoom(", scale.ToString(), ")"
				});
			} else {
				script = string.Concat(new string[] {
					"var point = new google.maps.LatLng(", CoordToStr(latitude), ",", CoordToStr(longitude), "); ", 
					"map.setCenter(point)"
				});
			}

			this.gm_ExecScript(script);
		}

		public void ZoomToBounds()
		{
			CoordsRect rt = this.GetPointsFrame();

			if (rt.MinLon != rt.MaxLon && rt.MinLat != rt.MaxLat)
			{
				double centerLongtude = ((rt.MaxLon + rt.MinLon) / 2.0);
				double centerLatitude = ((rt.MaxLat + rt.MinLat) / 2.0);

				string script = 
					"var point1 = new google.maps.LatLng({0}, {1});" +
					"var point2 = new google.maps.LatLng({2}, {3});" +
					"var bounds = new google.maps.LatLngBounds(point1, point2);" +
					"map.fitBounds(bounds);" +
					"map.setCenter(new google.maps.LatLng({4}, {5}));";
				script = string.Format(script, new object[]
					{ CoordToStr(rt.MinLat), CoordToStr(rt.MinLon), CoordToStr(rt.MaxLat), CoordToStr(rt.MaxLon), CoordToStr(centerLatitude), CoordToStr(centerLongtude) });

				this.gm_ExecScript(script);
			}
		}

		private static bool GetInetFile(string fileURL, ref Stream stream)
		{
			bool result;
			try
			{
				HttpWebRequest request = WebRequest.CreateDefault(new Uri(fileURL)) as HttpWebRequest;
				request.ContentType = "application/x-www-form-urlencoded";
				ProxyOptions proxy = TfmGEDKeeper.Instance.Options.Proxy;
				if (proxy.UseProxy)
				{
					request.Proxy = new WebProxy(proxy.Server + ":" + proxy.Port, true)
					{
						Credentials = CredentialCache.DefaultCredentials
					};
				}
				HttpWebResponse response = request.GetResponse() as HttpWebResponse;
				stream = response.GetResponseStream();
				result = true;
			}
			catch (Exception ex)
			{
                SysUtils.LogWrite("GKMapBrowser.GetInetFile(): " + ex.Message);
				stream = null;
				result = false;
			}
			return result;
		}

		public static void RequestGeoCoords(string searchValue, ExtList<GMapPoint> pointsList)
		{
            if (pointsList == null)
            {
                throw new ArgumentNullException("pointsList");
            }

            Stream stm = null;
            try
            {
            	searchValue = searchValue.Trim().Replace(" ", "+");

            	string netQuery = "http://maps.googleapis.com/maps/api/geocode/xml?address={0}&sensor=false&language=ru";
            	netQuery = string.Format(netQuery, new object[] { searchValue });

            	if (GKMapBrowser.GetInetFile(netQuery, ref stm))
            	{
            		XmlDocument xmlDocument = new XmlDocument();
            		xmlDocument.Load(stm);
            		XmlNode node = xmlDocument.DocumentElement;

            		if (node != null && node.ChildNodes.Count > 0)
            		{
            			int num = node.ChildNodes.Count;
            			for (int i = 0; i < num; i++)
            			{
            				XmlNode xNode = node.ChildNodes[i];
            				if (xNode.Name == "result")
            				{
            					XmlNode addressNode = xNode["formatted_address"];
            					XmlNode geometry = xNode["geometry"];
            					XmlNode pointNode = geometry["location"];

            					if (addressNode != null && pointNode != null)
            					{
            						string ptHint = addressNode.InnerText;
            						double ptLongitude = SysUtils.ParseFloat(pointNode["lng"].InnerText, -1.0);
            						double ptLatitude = SysUtils.ParseFloat(pointNode["lat"].InnerText, -1.0);

            						if (ptLatitude != -1.0 && ptLongitude != -1.0)
            						{
            							GMapPoint pt = new GMapPoint(ptLatitude, ptLongitude, ptHint);
            							pointsList.Add(pt);
            						}
            					}
            				}
            			}
            		}
            	}
            }
            finally
            {
            	if (stm != null) stm.Dispose();
            }
		}

		#region Google-specific

		private void gm_ClearPoints()
		{
			this.gm_ExecScript("clearOverlays();");
		}

		private void gm_ExecScript(string script)
		{
			if (script.Trim() != "")
			{
				try
				{
					mshtml.IHTMLWindow2 win = (mshtml.IHTMLWindow2)this.Document.Window.DomWindow;
                    if (win != null) {
                        win.execScript(script, "JavaScript");
                    }
				}
				catch (Exception ex)
				{
                    SysUtils.LogWrite("GKMapBrowser.gm_ExecScript(): " + ex.Message);
				}
			}
		}
		
		#endregion
	}
}
