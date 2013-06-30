using System;
using System.Collections.Generic;
using System.Globalization;

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

		public TColumnProps(byte colType, bool colActive)
		{
			this.colType = colType;
			this.colActive = colActive;
		}
	}

	public struct TColumnStatic
	{
		//public byte colType;
		public string colName;
		public TDataType dataType;
		public NumberFormatInfo nfi;
		public string format;
		public int width;
		public bool active;
	}

	public abstract class TListColumns
	{
		protected List<TColumnProps> FColumns;
		protected Type ColEnum;

		public List<TColumnStatic> ColumnStatics;
		
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

		protected abstract void InitColumnStatics();

		public TListColumns()
		{
			this.ColumnStatics = new List<TColumnStatic>();
		}

		public void Clear()
		{
			FColumns.Clear();
		}

		public void AddStatic(/*Enum colType*/ string colName, TDataType dataType, int width, bool defActive)
		{
			TColumnStatic cs = new TColumnStatic();

			//cs.colType = ((IConvertible)colType).ToByte(null);
			cs.colName = colName;
			cs.dataType = dataType;
			cs.width = width;
			cs.nfi = null;
			cs.format = null;
			cs.active = defActive;

			this.ColumnStatics.Add(cs);
		}

		public void AddStatic(/*Enum colType*/ string colName, TDataType dataType, int width, string format, NumberFormatInfo nfi, bool defActive)
		{
			TColumnStatic cs = new TColumnStatic();

			//cs.colType = ((IConvertible)colType).ToByte(null);
			cs.colName = colName;
			cs.dataType = dataType;
			cs.width = width;
			cs.nfi = nfi;
			cs.format = format;
			cs.active = defActive;

			this.ColumnStatics.Add(cs);
		}

		protected void InitData(Type colEnum)
		{
			ColEnum = colEnum;

			InitColumnStatics();

			FColumns = new List<TColumnProps>();
			foreach (Enum e in Enum.GetValues(ColEnum))
			{
				FColumns.Add(new TColumnProps());
			}
		}

		public void ResetDefaults()
		{
			foreach (Enum e in Enum.GetValues(ColEnum))
			{
				byte i = (e as IConvertible).ToByte(null);

				TColumnStatic cs = ColumnStatics[i];

				FColumns[i] = new TColumnProps(i, cs.active);
			}
		}

		public void CopyTo(TIndividualListColumns columns)
		{
			foreach (Enum e in Enum.GetValues(ColEnum))
			{
				byte i = (e as IConvertible).ToByte(null);

				TColumnProps col = this.FColumns[i];
				columns[i] = col;
			}
		}

		public void LoadFromFile(IniFile aIniFile, string section)
		{
			foreach (Enum e in Enum.GetValues(ColEnum))
			{
				byte i = (e as IConvertible).ToByte(null);

				TColumnStatic def_col = ColumnStatics[i];

				TColumnProps col = this.FColumns[i];
				col.colType = (byte)aIniFile.ReadInteger(section, "ColType_" + i.ToString(), i);
				col.colActive = aIniFile.ReadBool(section, "ColActive_" + i.ToString(), def_col.active);
				this.FColumns[i] = col;
			}
		}

		public void SaveToFile(IniFile aIniFile, string section)
		{
			foreach (Enum e in Enum.GetValues(ColEnum))
			{
				byte i = (e as IConvertible).ToByte(null);

				aIniFile.WriteInteger(section, "ColType_" + i.ToString(), this.FColumns[i].colType);
				aIniFile.WriteBool(section, "ColActive_" + i.ToString(), this.FColumns[i].colActive);
			}
		}
	}

}
