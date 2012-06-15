using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;

using Ext.Utils;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Lists
{

	public struct TColumnProps
	{
		public byte colType;
		public bool colActive;

		public TColumnProps(Enum colType, bool colActive)
		{
			this.colType = ((IConvertible)colType).ToByte(null);
			this.colActive = colActive;
		}
	}


	public abstract class TListColumns
	{
		protected List<TColumnProps> FColumns;
		protected TColumnProps[] DefColumns;
		protected Type ColEnum;

		public int Count
		{
			get { return FColumns.Count; }
		}

		public TColumnProps this[int index]
		{
			get { return FColumns[index]; }
			set { FColumns[index] = value; }
		}

		public Type ColumnEnum
		{
			get { return this.ColEnum; }
		}

		public void Clear()
		{
			FColumns.Clear();
		}

		protected virtual void InitDefaultColumns()
		{
			
		}

		public TListColumns()
		{
		}

		protected void InitData(Type colEnum)
		{
			ColEnum = colEnum;
			InitDefaultColumns();

			FColumns = new List<TColumnProps>();
			foreach (Enum e in Enum.GetValues(ColEnum))
			{
				FColumns.Add(new TColumnProps());
			}
		}

		public void SetDefaults()
		{
			foreach (Enum e in Enum.GetValues(ColEnum))
			{
				byte i = ((IConvertible)e).ToByte(null);

				FColumns[i] = DefColumns[i];
			}
		}

		public void CopyTo(TIndividualListColumns columns)
		{
			foreach (Enum e in Enum.GetValues(ColEnum))
			{
				byte i = ((IConvertible)e).ToByte(null);

				TColumnProps col = this.FColumns[i];
				columns[i] = col;
			}
		}

		public void LoadFromFile([In] IniFile aIniFile, string section)
		{
			foreach (Enum e in Enum.GetValues(ColEnum))
			{
				byte i = ((IConvertible)e).ToByte(null);

				TColumnProps def_col = DefColumns[i];
				TColumnProps col = this.FColumns[i];
				col.colType = (byte)aIniFile.ReadInteger(section, "ColType_" + i.ToString(), def_col.colType);
				col.colActive = aIniFile.ReadBool(section, "ColActive_" + i.ToString(), def_col.colActive);
				this.FColumns[i] = col;
			}
		}

		public void SaveToFile([In] IniFile aIniFile, string section)
		{
			foreach (Enum e in Enum.GetValues(ColEnum))
			{
				byte i = ((IConvertible)e).ToByte(null);

				aIniFile.WriteInteger(section, "ColType_" + i.ToString(), this.FColumns[i].colType);
				aIniFile.WriteBool(section, "ColActive_" + i.ToString(), this.FColumns[i].colActive);
			}
		}
	}

}
