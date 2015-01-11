using System;
using System.Drawing;
using ExtUtils;

/// <summary>
/// 
/// </summary>

namespace GKCore.Options
{
	public sealed class TreeChartOptions : IDisposable
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

        private bool fDisposed;


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

		public bool NickVisible
		{
			get;
			set;
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

		public TreeChartOptions()
		{
			this.FChildlessExclude = false;
			this.FDecorative = true;
			this.FFamilyVisible = true;
			this.FNameVisible = true;
			this.FPatronymicVisible = true;
			this.NickVisible = false;
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
			if (!this.fDisposed)
			{
				this.fDisposed = true;
			}
		}

		public void LoadFromFile(IniFile iniFile)
		{
		    if (iniFile == null) return;

			this.FChildlessExclude = iniFile.ReadBool("Chart", "ChildlessExclude", false);
			this.FDecorative = iniFile.ReadBool("Chart", "Decorative", true);
			this.FFamilyVisible = iniFile.ReadBool("Chart", "FamilyVisible", true);
			this.FNameVisible = iniFile.ReadBool("Chart", "NameVisible", true);
			this.FPatronymicVisible = iniFile.ReadBool("Chart", "PatronymicVisible", true);
			this.NickVisible = iniFile.ReadBool("Chart", "NickVisible", true);
			this.FDiffLines = iniFile.ReadBool("Chart", "DiffLines", false);
			this.FBirthDateVisible = iniFile.ReadBool("Chart", "BirthDateVisible", false);
			this.FDeathDateVisible = iniFile.ReadBool("Chart", "DeathDateVisible", false);
			this.FOnlyYears = iniFile.ReadBool("Chart", "OnlyYears", false);
			this.FKinship = iniFile.ReadBool("Chart", "Kinship", false);
			this.FSignsVisible = iniFile.ReadBool("Chart", "SignsVisible", false);
			this.FPortraitsVisible = iniFile.ReadBool("Chart", "PortraitsVisible", true);
			this.FMaleColor = Color.FromArgb(iniFile.ReadInteger("Chart", "MaleColor", -3750145));
			this.FFemaleColor = Color.FromArgb(iniFile.ReadInteger("Chart", "FemaleColor", -14650));
			this.FUnkSexColor = Color.FromArgb(iniFile.ReadInteger("Chart", "UnkSexColor", -14593));
			this.FUnHusbandColor = Color.FromArgb(iniFile.ReadInteger("Chart", "UnHusbandColor", -2631681));
			this.FUnWifeColor = Color.FromArgb(iniFile.ReadInteger("Chart", "UnWifeColor", -10281));
			this.FDefFont_Name = iniFile.ReadString("Chart", "FontName", "Verdana");
			this.FDefFont_Size = iniFile.ReadInteger("Chart", "FontSize", 8);
			this.FDefFont_Color = Color.FromArgb(iniFile.ReadInteger("Chart", "FontColor", Color.Black.ToArgb()));
			this.FDefFont_Style = (FontStyle)((uint)iniFile.ReadInteger("Chart", "FontStyle", 0));
		}

		public void SaveToFile(IniFile iniFile)
		{
            if (iniFile == null) return;

			iniFile.WriteBool("Chart", "ChildlessExclude", this.FChildlessExclude);
			iniFile.WriteBool("Chart", "Decorative", this.FDecorative);
			iniFile.WriteBool("Chart", "FamilyVisible", this.FFamilyVisible);
			iniFile.WriteBool("Chart", "NameVisible", this.FNameVisible);
			iniFile.WriteBool("Chart", "PatronymicVisible", this.FPatronymicVisible);
			iniFile.WriteBool("Chart", "NickVisible", this.NickVisible);
			iniFile.WriteBool("Chart", "DiffLines", this.FDiffLines);
			iniFile.WriteBool("Chart", "BirthDateVisible", this.FBirthDateVisible);
			iniFile.WriteBool("Chart", "DeathDateVisible", this.FDeathDateVisible);
			iniFile.WriteBool("Chart", "OnlyYears", this.FOnlyYears);
			iniFile.WriteBool("Chart", "Kinship", this.FKinship);
			iniFile.WriteBool("Chart", "SignsVisible", this.FSignsVisible);
			iniFile.WriteBool("Chart", "PortraitsVisible", this.FPortraitsVisible);
			iniFile.WriteInteger("Chart", "MaleColor", this.FMaleColor.ToArgb());
			iniFile.WriteInteger("Chart", "FemaleColor", this.FFemaleColor.ToArgb());
			iniFile.WriteInteger("Chart", "UnkSexColor", this.FUnkSexColor.ToArgb());
			iniFile.WriteInteger("Chart", "UnHusbandColor", this.FUnHusbandColor.ToArgb());
			iniFile.WriteInteger("Chart", "UnWifeColor", this.FUnWifeColor.ToArgb());
			iniFile.WriteString("Chart", "FontName", this.FDefFont_Name);
			iniFile.WriteInteger("Chart", "FontSize", this.FDefFont_Size);
			iniFile.WriteInteger("Chart", "FontColor", this.FDefFont_Color.ToArgb());
			iniFile.WriteInteger("Chart", "FontStyle", (byte)this.FDefFont_Style);
		}

	}
}
