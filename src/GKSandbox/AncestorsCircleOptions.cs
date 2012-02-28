using System;
using System.Drawing;

/// <summary>
/// Localization: clean
/// CodeTransformation: need
/// </summary>

namespace GKSandbox
{
	public class AncestorsCircleOptions
	{
		public Color[] BrushColor = new Color[11];
		public SolidBrush[] CircleBrushes = new SolidBrush[11];
		public bool FirstName = true;
		public bool CircularLines = true;

		public AncestorsCircleOptions()
		{
			this.CircularLines = true;
			CreateBrushes();
		}
		
		public void CreateBrushes()
		{
			this.BrushColor[ 0] = Color.Coral;
			this.BrushColor[ 1] = Color.CadetBlue;
			this.BrushColor[ 2] = Color.DarkGray;
			this.BrushColor[ 3] = Color.Khaki;
			this.BrushColor[ 4] = Color.CadetBlue;//LawnGreen;
			this.BrushColor[ 5] = Color.DarkGray;//Khaki;
			this.BrushColor[ 6] = Color.Khaki;//HotPink;
			this.BrushColor[ 7] = Color.CadetBlue;//Ivory;

			this.BrushColor[ 8] = Color.Black; // text
			this.BrushColor[ 9] = Color.Moccasin; // background
			this.BrushColor[10] = Color.Black; // lines

			for (int i = 0; i <= this.BrushColor.Length - 1; i++)
			{
				this.CircleBrushes[i] = new SolidBrush(this.BrushColor[i]);
			}
		}
	}
}
