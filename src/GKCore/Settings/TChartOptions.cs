using System;
using System.ComponentModel;
using System.Drawing;
using System.Runtime.InteropServices;

using GKCore.Sys;

namespace GKCore
{
	public class TChartOptions : IDisposable
	{
		private bool FChildlessExclude;
		private bool FDecorative;
		private bool FFamilyVisible;
		private bool FNameVisible;
		private bool FPatronymicVisible;
		private bool FDiffLines;
		private bool FBirthDateVisible;
		private bool FDeathDateVisible;
		private bool FOnlyYears;
		private bool FKinship;
		private bool FPortraitsVisible;
		private bool FSignsVisible;
		private Color FMaleColor;
		private Color FFemaleColor;
		private Color FUnkSexColor;
		private Color FUnHusbandColor;
		private Color FUnWifeColor;
		private string FDefFont_Name;
		private int FDefFont_Size;
		private Color FDefFont_Color;
		private FontStyle FDefFont_Style;
		protected bool Disposed_;


		public bool ChildlessExclude
		{
			get { return this.FChildlessExclude; }
			set { this.FChildlessExclude = value; }
		}

		public bool Decorative
		{
			get { return this.FDecorative; }
			set { this.FDecorative = value; }
		}

		public bool FamilyVisible
		{
			get { return this.FFamilyVisible; }
			set { this.FFamilyVisible = value; }
		}

		public bool NameVisible
		{
			get { return this.FNameVisible; }
			set { this.FNameVisible = value; }
		}

		public bool PatronymicVisible
		{
			get { return this.FPatronymicVisible; }
			set { this.FPatronymicVisible = value; }
		}

		public bool DiffLines
		{
			get { return this.FDiffLines; }
			set { this.FDiffLines = value; }
		}

		public bool BirthDateVisible
		{
			get { return this.FBirthDateVisible; }
			set { this.FBirthDateVisible = value; }
		}

		public bool DeathDateVisible
		{
			get { return this.FDeathDateVisible; }
			set { this.FDeathDateVisible = value; }
		}

		public bool OnlyYears
		{
			get { return this.FOnlyYears; }
			set { this.FOnlyYears = value; }
		}

		public bool Kinship
		{
			get { return this.FKinship; }
			set { this.FKinship = value; }
		}

		public bool PortraitsVisible
		{
			get { return this.FPortraitsVisible; }
			set { this.FPortraitsVisible = value; }
		}

		public bool SignsVisible
		{
			get { return this.FSignsVisible; }
			set { this.FSignsVisible = value; }
		}

		public Color MaleColor
		{
			get { return this.FMaleColor; }
			set { this.FMaleColor = value; }
		}

		public Color FemaleColor
		{
			get { return this.FFemaleColor; }
			set { this.FFemaleColor = value; }
		}

		public Color UnkSexColor
		{
			get { return this.FUnkSexColor; }
			set { this.FUnkSexColor = value; }
		}

		public Color UnHusbandColor
		{
			get { return this.FUnHusbandColor; }
			set { this.FUnHusbandColor = value; }
		}

		public Color UnWifeColor
		{
			get { return this.FUnWifeColor; }
			set { this.FUnWifeColor = value; }
		}

		public string DefFont_Name
		{
			get { return this.FDefFont_Name; }
			set { this.FDefFont_Name = value; }
		}

		public int DefFont_Size
		{
			get { return this.FDefFont_Size; }
			set { this.FDefFont_Size = value; }
		}

		public Color DefFont_Color
		{
			get { return this.FDefFont_Color; }
			set { this.FDefFont_Color = value; }
		}

		public FontStyle DefFont_Style
		{
			get { return this.FDefFont_Style; }
			set { this.FDefFont_Style = value; }
		}

		public TChartOptions()
		{
			this.FChildlessExclude = false;
			this.FDecorative = true;
			this.FFamilyVisible = true;
			this.FNameVisible = true;
			this.FPatronymicVisible = true;
			this.FDiffLines = false;
			this.FBirthDateVisible = false;
			this.FDeathDateVisible = false;
			this.FOnlyYears = false;
			this.FKinship = false;
			this.FPortraitsVisible = true;
			this.FSignsVisible = false;
			this.FMaleColor = Color.FromArgb(-3750145);
			this.FFemaleColor = Color.FromArgb(-14650);
			this.FUnkSexColor = Color.FromArgb(-14593);
			this.FUnHusbandColor = Color.FromArgb(-2631681);
			this.FUnWifeColor = Color.FromArgb(-10281);
			this.FDefFont_Name = "Verdana";
			this.FDefFont_Size = 8;
			this.FDefFont_Color = Color.Black;
			this.FDefFont_Style = FontStyle.Regular;
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				this.Disposed_ = true;
			}
		}

		public void LoadFromFile([In] TIniFile aIniFile)
		{
			this.FChildlessExclude = aIniFile.ReadBool("Chart", "ChildlessExclude", false);
			this.FDecorative = aIniFile.ReadBool("Chart", "Decorative", true);
			this.FFamilyVisible = aIniFile.ReadBool("Chart", "FamilyVisible", true);
			this.FNameVisible = aIniFile.ReadBool("Chart", "NameVisible", true);
			this.FPatronymicVisible = aIniFile.ReadBool("Chart", "PatronymicVisible", true);
			this.FDiffLines = aIniFile.ReadBool("Chart", "DiffLines", false);
			this.FBirthDateVisible = aIniFile.ReadBool("Chart", "BirthDateVisible", false);
			this.FDeathDateVisible = aIniFile.ReadBool("Chart", "DeathDateVisible", false);
			this.FOnlyYears = aIniFile.ReadBool("Chart", "OnlyYears", false);
			this.FKinship = aIniFile.ReadBool("Chart", "Kinship", false);
			this.FSignsVisible = aIniFile.ReadBool("Chart", "SignsVisible", false);
			this.FPortraitsVisible = aIniFile.ReadBool("Chart", "PortraitsVisible", true);
			this.FMaleColor = Color.FromArgb(aIniFile.ReadInteger("Chart", "MaleColor", -3750145));
			this.FFemaleColor = Color.FromArgb(aIniFile.ReadInteger("Chart", "FemaleColor", -14650));
			this.FUnkSexColor = Color.FromArgb(aIniFile.ReadInteger("Chart", "UnkSexColor", -14593));
			this.FUnHusbandColor = Color.FromArgb(aIniFile.ReadInteger("Chart", "UnHusbandColor", -2631681));
			this.FUnWifeColor = Color.FromArgb(aIniFile.ReadInteger("Chart", "UnWifeColor", -10281));
			this.FDefFont_Name = aIniFile.ReadString("Chart", "FontName", "Verdana");
			this.FDefFont_Size = aIniFile.ReadInteger("Chart", "FontSize", 8);
			this.FDefFont_Color = Color.FromArgb(aIniFile.ReadInteger("Chart", "FontColor", Color.Black.ToArgb()));
			this.FDefFont_Style = (FontStyle)((uint)((byte)aIniFile.ReadInteger("Chart", "FontStyle", 0)));
		}

		public void SaveToFile([In] TIniFile aIniFile)
		{
			aIniFile.WriteBool("Chart", "ChildlessExclude", this.FChildlessExclude);
			aIniFile.WriteBool("Chart", "Decorative", this.FDecorative);
			aIniFile.WriteBool("Chart", "FamilyVisible", this.FFamilyVisible);
			aIniFile.WriteBool("Chart", "NameVisible", this.FNameVisible);
			aIniFile.WriteBool("Chart", "PatronymicVisible", this.FPatronymicVisible);
			aIniFile.WriteBool("Chart", "DiffLines", this.FDiffLines);
			aIniFile.WriteBool("Chart", "BirthDateVisible", this.FBirthDateVisible);
			aIniFile.WriteBool("Chart", "DeathDateVisible", this.FDeathDateVisible);
			aIniFile.WriteBool("Chart", "OnlyYears", this.FOnlyYears);
			aIniFile.WriteBool("Chart", "Kinship", this.FKinship);
			aIniFile.WriteBool("Chart", "SignsVisible", this.FSignsVisible);
			aIniFile.WriteBool("Chart", "PortraitsVisible", this.FPortraitsVisible);
			aIniFile.WriteInteger("Chart", "MaleColor", this.FMaleColor.ToArgb());
			aIniFile.WriteInteger("Chart", "FemaleColor", this.FFemaleColor.ToArgb());
			aIniFile.WriteInteger("Chart", "UnkSexColor", this.FUnkSexColor.ToArgb());
			aIniFile.WriteInteger("Chart", "UnHusbandColor", this.FUnHusbandColor.ToArgb());
			aIniFile.WriteInteger("Chart", "UnWifeColor", this.FUnWifeColor.ToArgb());
			aIniFile.WriteString("Chart", "FontName", this.FDefFont_Name);
			aIniFile.WriteInteger("Chart", "FontSize", this.FDefFont_Size);
			aIniFile.WriteInteger("Chart", "FontColor", this.FDefFont_Color.ToArgb());
			aIniFile.WriteInteger("Chart", "FontStyle", (int)((byte)this.FDefFont_Style));
		}

		public void Free()
		{
			TObjectHelper.Free(this);
		}

	}
}
