using System;
using System.Collections.Generic;
using System.Globalization;

using GKCommon;
using GKCore.Interfaces;

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

    /// <summary>
    /// 
    /// </summary>
    public abstract class ListColumns
	{
		protected List<TColumnProps> fColumns;
        private Type fColumnEnum;

		public List<TColumnStatic> ColumnStatics;
		
		public int Count
		{
			get { return fColumns.Count; }
		}

		public TColumnProps this[int index]
		{
			get { return fColumns[index]; }
			set { fColumns[index] = value; }
		}

		/*public Type ColumnEnum
		{
            get { return this.fColumnEnum; }
		}*/

		protected abstract void InitColumnStatics();

	    protected ListColumns()
		{
			this.ColumnStatics = new List<TColumnStatic>();
		}

		public void Clear()
		{
			fColumns.Clear();
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
            this.fColumnEnum = colEnum;

			InitColumnStatics();

			fColumns = new List<TColumnProps>();
            foreach (Enum e in Enum.GetValues(fColumnEnum))
			{
				fColumns.Add(new TColumnProps());
			}
		}

		public void ResetDefaults()
		{
            foreach (Enum e in Enum.GetValues(fColumnEnum))
			{
				byte i = (e as IConvertible).ToByte(null);

				TColumnStatic cs = ColumnStatics[i];

				fColumns[i] = new TColumnProps(i, cs.active);
			}
		}

		public void CopyTo(IndividualListColumns columns)
		{
            foreach (Enum e in Enum.GetValues(fColumnEnum))
			{
				byte i = (e as IConvertible).ToByte(null);

				TColumnProps col = this.fColumns[i];
				columns[i] = col;
			}
		}

		public void LoadFromFile(IniFile iniFile, string section)
		{
		    if (iniFile == null) return;

            foreach (Enum e in Enum.GetValues(fColumnEnum))
			{
				byte i = (e as IConvertible).ToByte(null);

				TColumnStatic defCol = ColumnStatics[i];

				TColumnProps col = this.fColumns[i];
				col.colType = (byte)iniFile.ReadInteger(section, "ColType_" + i.ToString(), i);
				col.colActive = iniFile.ReadBool(section, "ColActive_" + i.ToString(), defCol.active);
				this.fColumns[i] = col;
			}
		}

		public void SaveToFile(IniFile iniFile, string section)
		{
            if (iniFile == null) return;

            foreach (Enum e in Enum.GetValues(fColumnEnum))
			{
				byte i = (e as IConvertible).ToByte(null);

				iniFile.WriteInteger(section, "ColType_" + i.ToString(), this.fColumns[i].colType);
				iniFile.WriteBool(section, "ColActive_" + i.ToString(), this.fColumns[i].colActive);
			}
		}
	}

}
