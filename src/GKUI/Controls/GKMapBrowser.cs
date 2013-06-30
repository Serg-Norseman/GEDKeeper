using System;
using System.Globalization;
using System.IO;
using System.Net;
using System.Text;
using System.Windows.Forms;
using System.Xml;

using Ext.Utils;
using GKCore;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Controls
{
	public class GKMapBrowser : WebBrowser, IDisposable
	{
		public class TGMapPoint
		{
			private DateTime FDate;
			private double FLatitude;
			private double FLongitude;
			private string FHint;

			public DateTime Date
			{
				get { return this.FDate; }
				set { this.FDate = value; }
			}

			public double Latitude
			{
				get { return this.FLatitude; }
				set { this.FLatitude = value; }
			}

			public double Longitude
			{
				get { return this.FLongitude; }
				set { this.FLongitude = value; }
			}

			public string Hint
			{
				get { return this.FHint; }
				set { this.FHint = value; }
			}

			public TGMapPoint()
			{
			}

			public TGMapPoint(double aLatitude, double aLongitude, string aHint)
			{
				this.FLatitude = aLatitude;
				this.FLongitude = aLongitude;
				this.FHint = aHint;
			}

			public void Free()
			{
				SysUtils.Free(this);
			}
		}


		private struct TCoordsRect
		{
			public double MinLon;
			public double MinLat;
			public double MaxLon;
			public double MaxLat;
		}

		private static readonly string MapContent;
		private string FMapFile;
		private TList FMapPoints;
		private bool FShowPoints;
		private bool FShowLines;
		private int FUpdateCount;
		private static XmlDocument xmlDocument;

		public bool ShowPoints
		{
			get { return this.FShowPoints; }
			set { this.SetVisibleElementes(0, value); }
		}

		public bool ShowLines
		{
			get { return this.FShowLines; }
			set { this.SetVisibleElementes(1, value); }
		}

		private void gm_ClearPoints()
		{
			this.gm_ExecScript("map.clearOverlays();");
		}

		private void gm_ExecScript(string Script)
		{
			if (Script.Trim() != "")
			{
				try
				{
					mshtml.IHTMLWindow2 win = (mshtml.IHTMLWindow2)this.Document.Window.DomWindow;
					win.execScript(Script, "JavaScript");
				}
				catch (Exception E)
				{
					SysUtils.LogWrite("TMapBrowser.gm_ExecScript(): " + E.Message);
				}
			}
		}

		private static string FillNode(XmlNode Node)
		{
			string Result = "";
			if (Node.ChildNodes != null && Node.ChildNodes.Count > 0)
			{
				int num = Node.ChildNodes.Count - 1;
				int i = 0;
				if (num >= i)
				{
					num++;
					while (true)
					{
						GKMapBrowser.FillNode(Node.ChildNodes[i]);
						if (Node.Name == "address") {
							break;
						}
						i++;
						if (i == num) {
							return Result;
						}
					}
					Result = Node.InnerText;
				}
			}
			return Result;
		}

		public GKMapBrowser.TGMapPoint GetMapPoint(int Index)
		{
			GKMapBrowser.TGMapPoint Result = null;
			if (Index >= 0 && Index < this.FMapPoints.Count)
			{
				Result = (this.FMapPoints[Index] as GKMapBrowser.TGMapPoint);
			}
			return Result;
		}

		public int GetMapPointsCount()
		{
			return this.FMapPoints.Count;
		}

		private GKMapBrowser.TCoordsRect GetPointsFrame()
		{
			GKMapBrowser.TCoordsRect Result = new TCoordsRect();
			if (this.FMapPoints.Count > 0)
			{
				GKMapBrowser.TGMapPoint pt = this.FMapPoints[0] as GKMapBrowser.TGMapPoint;
				Result.MinLon = pt.Longitude;
				Result.MaxLon = pt.Longitude;
				Result.MinLat = pt.Latitude;
				Result.MaxLat = pt.Latitude;
				if (this.FMapPoints.Count == 1)
				{
					Result.MinLon = (Result.MinLon - 20.0);
					Result.MaxLon = (Result.MaxLon + 20.0);
					Result.MinLat = (Result.MinLat - 20.0);
					Result.MaxLat = (Result.MaxLat + 20.0);
				}
				else
				{
					int num = this.FMapPoints.Count - 1;
					for (int i = 0; i <= num; i++)
					{
						pt = (this.FMapPoints[i] as GKMapBrowser.TGMapPoint);

						if (Result.MinLon > pt.Longitude) Result.MinLon = pt.Longitude;
						else if (Result.MaxLon < pt.Longitude) Result.MaxLon = pt.Longitude;

						if (Result.MinLat > pt.Latitude) Result.MinLat = pt.Latitude;
						else if (Result.MaxLat < pt.Latitude) Result.MaxLat = pt.Latitude;
					}
				}
			}
			return Result;
		}

		private void SetVisibleElementes(int Index, bool Value)
		{
			if (Index != 0)
			{
				if (Index == 1)
				{
					this.FShowLines = Value;
				}
			}
			else
			{
				this.FShowPoints = Value;
			}
			this.RefreshPoints();
		}

		public GKMapBrowser()
		{
			this.FMapPoints = new TList(true);
			this.FUpdateCount = 0;
			this.FShowPoints = true;
			this.FShowLines = true;
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				File.Delete(this.FMapFile);
				this.ClearPoints();
				this.FMapPoints.Dispose();
			}
			base.Dispose(Disposing);
		}

		public int AddPoint(double aLatitude, double aLongitude, string aHint)
		{
			GKMapBrowser.TGMapPoint pt = new GKMapBrowser.TGMapPoint(aLatitude, aLongitude, aHint);
			return this.FMapPoints.Add(pt);
		}

		public void BeginUpdate()
		{
			this.FUpdateCount++;
		}

		public void ClearPoints()
		{
			this.gm_ClearPoints();
			this.FMapPoints.Clear();
		}

		public void DeletePoint(int aIndex)
		{
			this.FMapPoints.Delete(aIndex);
			this.RefreshPoints();
		}

		public void EndUpdate()
		{
			this.FUpdateCount--;
			if (this.FUpdateCount <= 0)
			{
				this.RefreshPoints();
				this.FUpdateCount = 0;
			}
		}

		public void InitMap()
		{
			this.FMapFile = TGenEngine.GetTempDir() + "\\GEDKeeperMap.html";
			StreamWriter tf = new StreamWriter(this.FMapFile, false, Encoding.UTF8);
			tf.WriteLine(GKMapBrowser.MapContent);
			tf.Close();

			Navigate(FMapFile);
		}

		public static string CoordToStr(double val) {
			NumberFormatInfo nfi = new NumberFormatInfo();
			nfi.NumberDecimalSeparator = ".";
			return val.ToString("0.000000", nfi);
		}

		public void RefreshPoints()
		{
			this.gm_ClearPoints();
			if (this.FMapPoints.Count > 0)
			{
				string PointsScript = "";
				string PolylineScript = "";

				for (int i = 0; i <= this.FMapPoints.Count - 1; i++)
				{
					TGMapPoint pt = this.FMapPoints[i] as TGMapPoint;
					PointsScript += string.Format("addMarker({0}, {1}, \"{2}\");", new object[]
					{ CoordToStr(pt.Latitude), CoordToStr(pt.Longitude), pt.Hint });

					PolylineScript = string.Concat(new string[]
					{
						PolylineScript, "new GLatLng(",	
						CoordToStr(pt.Latitude), ",", CoordToStr(pt.Longitude), "),"
					});
				}

				if (this.ShowPoints)
				{
					this.gm_ExecScript(PointsScript);
				}

				if (this.ShowLines)
				{
					int num2 = (PolylineScript != null) ? PolylineScript.Length : 0;
					PolylineScript = PolylineScript.Remove(num2 - 1, 1);
					PolylineScript = "var polyline = new GPolyline([" + PolylineScript + "],\"#FF0000\",3);map.addOverlay(polyline);";
					this.gm_ExecScript(PolylineScript);
				}
			}
		}

		public void SaveSnapshot(string aFileName)
		{
		}

		public void SetCenter(double aLatitude, double aLongitude, int aScale)
		{
			string Script;
			if (aScale >= 0) {
				Script = string.Concat(new string[] {
					"var point = new GLatLng(", CoordToStr(aLatitude), ",", CoordToStr(aLongitude), "); ", 
					"map.setCenter(point, ", aScale.ToString(), ")"
				});
			} else {
				Script = string.Concat(new string[] {
					"var point = new GLatLng(", CoordToStr(aLatitude), ",", CoordToStr(aLongitude), "); ", 
					"map.setCenter(point)"
				});
			}

			this.gm_ExecScript(Script);
		}

		public void ZoomToBounds()
		{
			GKMapBrowser.TCoordsRect rt = this.GetPointsFrame();
			if (rt.MinLon != rt.MaxLon && rt.MinLat != rt.MaxLat)
			{
				double Center_Longtude = ((rt.MaxLon + rt.MinLon) / 2.0);
				double Center_Latitude = ((rt.MaxLat + rt.MinLat) / 2.0);

				string Script = "var point1 = new GLatLng({0}, {1});var point2 = new GLatLng({2}, {3});var bounds = new GLatLngBounds(point1, point2);var zoom = map.getBoundsZoomLevel(bounds);map.setCenter(new GLatLng({4}, {5}), zoom);";
				Script = string.Format(Script, new object[]
				{ CoordToStr(rt.MinLat), CoordToStr(rt.MinLon), CoordToStr(rt.MaxLat), CoordToStr(rt.MaxLon), CoordToStr(Center_Latitude), CoordToStr(Center_Longtude) });

				this.gm_ExecScript(Script);
			}
		}

		public static void GeoInit()
		{
			GKMapBrowser.xmlDocument = new XmlDocument();
		}

		public static void GeoDone()
		{
			GKMapBrowser.xmlDocument = null;
		}

		public static bool GetInetFile(string FileURL, ref Stream Stream)
		{
			bool Result;
			try
			{
				HttpWebRequest request = WebRequest.CreateDefault(new Uri(FileURL)) as HttpWebRequest;
				request.ContentType = "application/x-www-form-urlencoded";
				ProxyOptions proxy = GKUI.TfmGEDKeeper.Instance.Options.Proxy;
				if (proxy.UseProxy)
				{
					request.Proxy = new WebProxy(proxy.Server + ":" + proxy.Port, true)
					{
						Credentials = CredentialCache.DefaultCredentials
					};
				}
				HttpWebResponse response = request.GetResponse() as HttpWebResponse;
				Stream = response.GetResponseStream();
				Result = true;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GetInetFile(): " + E.Message);
				Stream = null;
				Result = false;
			}
			return Result;
		}

		public static void RequestGeoCoords(string SearchString, TList aPoints)
		{
			if (aPoints != null)
			{
				Stream stm = null;
				try
				{
					SearchString = SearchString.Trim().Replace(" ", "+");

					string FileOnNet = "http://maps.google.ru/maps/geo?q={0}&output=xml&key={1}&gl=ru";
					FileOnNet = string.Format(FileOnNet, new object[]
					{
						SearchString, 
						"ABQIAAAAIcIQgkzLQ27NamNDh2wULxTh9o9-e_HqfKVqUrQPniGEP9J6uhSJmXGEipvip6lxpu_ZXrXaeHwWgQ"
					});
					if (GKMapBrowser.GetInetFile(FileOnNet, ref stm))
					{
						GKMapBrowser.xmlDocument.Load(stm);
						XmlNode Node = GKMapBrowser.xmlDocument.DocumentElement;
						Node = Node["Response"];
						if (Node != null && Node.ChildNodes.Count > 0)
						{
							int num = Node.ChildNodes.Count - 1;
							for (int i = 0; i <= num; i++)
							{
								if (Node.ChildNodes[i].Name == "Placemark")
								{
									XmlNode PlacemarkNode = Node.ChildNodes[i];
									XmlNode AddressNode = PlacemarkNode["address"];
									XmlNode PointNode = PlacemarkNode["Point"];
									PointNode = PointNode["coordinates"];
									if (AddressNode != null && PointNode != null)
									{
										GKMapBrowser.TGMapPoint pt = new GKMapBrowser.TGMapPoint();
										pt.Hint = AddressNode.InnerText;
										string coords = PointNode.InnerText;
										string[] coord_tokens = coords.Split(new char[] {','});
										if (((coord_tokens != null) ? coord_tokens.Length : 0) > 1)
										{
											pt.Longitude = SysUtils.ParseFloat(coord_tokens[0], -1.0);
											pt.Latitude = SysUtils.ParseFloat(coord_tokens[1], -1.0);
											if (pt.Latitude != -1.0 && pt.Longitude != -1.0) aPoints.Add(pt);
										}
										else
										{
											pt.Free();
										}
									}
								}
							}
						}
					}
				}
				finally
				{
					//stm.Dispose; checkit
				}
			}
		}

		public static string RequestGeoAddress(double aLatitude, double aLongitude)
		{
			string Result = "";
			Stream stm = null;
			try
			{
				string FileOnNet = "http://maps.google.ru/maps/geo?ll={0},{1}&output=xml&key={2}&gl=ru";
				FileOnNet = string.Format(FileOnNet, new object[]
				{
				    CoordToStr(aLatitude), CoordToStr(aLongitude),
					"ABQIAAAAIcIQgkzLQ27NamNDh2wULxTh9o9-e_HqfKVqUrQPniGEP9J6uhSJmXGEipvip6lxpu_ZXrXaeHwWgQ"
				});
				if (GKMapBrowser.GetInetFile(FileOnNet, ref stm))
				{
					GKMapBrowser.xmlDocument.Load(stm);
					Result = GKMapBrowser.FillNode(GKMapBrowser.xmlDocument.DocumentElement);
				}
			}
			finally
			{
				//stm.Dispose; //checkit
			}
			return Result;
		}

		static GKMapBrowser()
		{
			GKMapBrowser.MapContent = "<html>\r\n  <head>\r\n    <meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\"/>\r\n    <title>Map</title>\r\n    <script src=\"http://maps.google.ru/maps?file=api&amp;v=2&amp;key=ABQIAAAAIcIQgkzLQ27NamNDh2wULxTh9o9-e_HqfKVqUrQPniGEP9J6uhSJmXGEipvip6lxpu_ZXrXaeHwWgQ\" type=\"text/javascript\"></script>\r\n    <script type=\"text/javascript\">\r\n    var map;\r\n    function addMarker(latitude, longitude, hint)\r\n    {\r\n        var blueIcon = new GIcon(G_DEFAULT_ICON);\r\n        markerOptions = { icon : blueIcon, title : hint };\r\n        var latlng = new GLatLng(latitude,longitude);\r\n        map.addOverlay(new GMarker(latlng, markerOptions));\r\n    }\r\n    function initialize() {\r\n      if (GBrowserIsCompatible()) {\r\n        map = new GMap2(document.getElementById(\"map\"));\r\n        map.addMapType(G_PHYSICAL_MAP);\r\n        map.setCenter(new GLatLng(55.755786, 37.617633), 11, G_PHYSICAL_MAP);\r\n        map.addControl(new GLargeMapControl());\r\n        map.addControl(new GMapTypeControl());\r\n      }\r\n    }\r\n    </script>\r\n  </head>\r\n  <body onload=\"initialize()\" onunload=\"GUnload()\">\r\n    <div id=\"map\" style=\"position:absolute; width: 100%; height: 100%; left: 0px; top: 0px;\"></div>\r\n    <noscript><b style=\"font-family:Tahoma;\">JavaScript must be switched on for use Google Maps.</b></noscript>\r\n  </body>\r\n</html>";
		}
	}
}
