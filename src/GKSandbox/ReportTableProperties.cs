using System;
using System.Collections.Generic;
using System.Drawing;

/// <summary>
/// Localization: unknown
/// CodeTransformation: need
/// </summary>

namespace GKSandbox
{
	public class TableHeader
	{
		public string Name;
		public string Alias;
		public string Value;
		public float Width;
		public int Alignment;

		public TableHeader(string name, float width, int alignment)
		{
			this.Name = name;
			this.Alias = this.Name;
			this.Width = width;
			this.Alignment = alignment;
			this.Value = "";
		}
	}

	public class ReportTableProperties
	{
		public float Padding = 2f;
		public float Spacing;
		public float CellPadding = 2f;
		public float CellSpacing;
		public float SpaceInsideCell = 2f;
		public float BorderWidth = 1f;
		public bool ShowHeadersOnEveryPage = true;
		public Color BorderColor = Color.Black;
		public Color HeaderBackColor = Color.CornflowerBlue;
		public Color HeaderForeColor = Color.White;
		public List<TableHeader> fields1;
		public List<TableHeader> fields2;
		public List<TableHeader> fields3;

		public ReportTableProperties()
		{
			this.fields1 = new List<TableHeader>();
			this.fields1.Add(new TableHeader("ID", 5f, 2));
			this.fields1.Add(new TableHeader("FirstLastName", 43f, 0));
			this.fields1.Add(new TableHeader("Date of Birth", 7f, 1));
			this.fields1.Add(new TableHeader("Place of Birth", 25f, 0));
			this.fields1.Add(new TableHeader("Path", 20f, 0));

			this.fields2 = new List<TableHeader>();
			this.fields2.Add(new TableHeader("ID", 5f, 2));
			this.fields2.Add(new TableHeader("FirstLastName", 43f, 0));
			this.fields2.Add(new TableHeader("Date of Birth", 7f, 1));
			this.fields2.Add(new TableHeader("Place of Birth", 25f, 0));
			this.fields2.Add(new TableHeader("Comments", 20f, 0));

			this.fields3 = new List<TableHeader>();
			this.fields3.Add(new TableHeader("ID", 5f, 2));
			this.fields3.Add(new TableHeader("LongName", 33f, 0));
			this.fields3.Add(new TableHeader("Date", 7f, 1));
			this.fields3.Add(new TableHeader("Place of Marriage", 20f, 0));
			this.fields3.Add(new TableHeader("Children", 15f, 0));
			this.fields3.Add(new TableHeader("Comments", 20f, 0));
		}

		public float[] GetFields1Widths()
		{
			float[] array = new float[this.fields1.Count];
			for (int i = 0; i < this.fields1.Count; i++)
			{
				array[i] = this.fields1[i].Width;
			}
			return array;
		}

		public float[] GetFields2Widths()
		{
			float[] array = new float[this.fields2.Count];
			for (int i = 0; i < this.fields2.Count; i++)
			{
				array[i] = this.fields2[i].Width;
			}
			return array;
		}

		public float[] GetFields3Widths()
		{
			float[] array = new float[this.fields3.Count];
			for (int i = 0; i < this.fields3.Count; i++)
			{
				array[i] = this.fields3[i].Width;
			}
			return array;
		}

		public bool HasValuesFields1()
		{
			bool result = false;
			foreach (TableHeader current in this.fields1)
			{
				if (current.Value.Length > 0)
				{
					result = true;
				}
			}
			return result;
		}

		public bool HasValuesFields2()
		{
			bool result = false;
			foreach (TableHeader current in this.fields2)
			{
				if (current.Value.Length > 0)
				{
					result = true;
				}
			}
			return result;
		}
	}
}
