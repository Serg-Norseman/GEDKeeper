using System;

namespace GKCore.Maps
{
	public class GMapPoint
	{
		public double Latitude { get; set; }
		public double Longitude { get; set; }
		public string Hint { get; set; }
		public DateTime Date { get; set; }

		public GMapPoint(double latitude, double longitude, string hint)
		{
			this.Latitude = latitude;
			this.Longitude = longitude;
			this.Hint = hint;
		}
	}
}
