using System;
using System.Collections.Generic;
using System.Globalization;

using GKCommon;
using GKCore;
using GKCore.Interfaces;

namespace GKUI.Lists
{
	public sealed class ColumnProps
	{
		public byte colType;
		public bool colActive;
		public int colWidth;

		public ColumnProps()
		{
		}

		public ColumnProps(byte colType, bool colActive, int colWidth)
		{
			this.colType = colType;
			this.colActive = colActive;
			this.colWidth = colWidth;
		}
	}

	public sealed class ColumnStatic
	{
		//public byte colType;
		public LSID colName;
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
		private List<ColumnProps> fColumns;
        private Type fColumnEnum;

		public List<ColumnStatic> ColumnStatics;
		
		public int Count
		{
			get { return fColumns.Count; }
		}

		public ColumnProps this[int index]
		{
			get { return fColumns[index]; }
			set { fColumns[index] = value; }
		}

		public Type ColumnEnum
		{
            get { return this.fColumnEnum; }
		}

		protected abstract void InitColumnStatics();

	    protected ListColumns()
		{
			this.ColumnStatics = new List<ColumnStatic>();
		}

		protected void InitData(Type colEnum)
		{
            this.fColumnEnum = colEnum;

			InitColumnStatics();

			this.fColumns = new List<ColumnProps>();
            foreach (Enum e in Enum.GetValues(this.fColumnEnum))
			{
				this.fColumns.Add(new ColumnProps());
			}
		}

		public void ResetDefaults()
		{
            foreach (Enum e in Enum.GetValues(this.fColumnEnum))
			{
				byte i = (e as IConvertible).ToByte(null);

				ColumnStatic cs = ColumnStatics[i];

				fColumns[i] = new ColumnProps(i, cs.active, cs.width);
			}
		}

		public void Clear()
		{
			this.fColumns.Clear();
		}

		public void AddStatic(/*Enum colType*/ LSID colName, TDataType dataType, int defWidth, bool defActive)
		{
			ColumnStatic cs = new ColumnStatic();

			//cs.colType = ((IConvertible)colType).ToByte(null);
			cs.colName = colName;
			cs.dataType = dataType;
			cs.width = defWidth;
			cs.nfi = null;
			cs.format = null;
			cs.active = defActive;

			this.ColumnStatics.Add(cs);
		}

		public void AddStatic(/*Enum colType*/ LSID colName, TDataType dataType, int defWidth, bool defActive, string format, NumberFormatInfo nfi)
		{
			ColumnStatic cs = new ColumnStatic();

			//cs.colType = ((IConvertible)colType).ToByte(null);
			cs.colName = colName;
			cs.dataType = dataType;
			cs.width = defWidth;
			cs.nfi = nfi;
			cs.format = format;
			cs.active = defActive;

			this.ColumnStatics.Add(cs);
		}

		public void CopyTo(ListColumns columns)
		{
            if (columns == null) {
                throw new ArgumentNullException("columns");
            }

            foreach (Enum e in Enum.GetValues(this.fColumnEnum))
			{
				byte i = (e as IConvertible).ToByte(null);

				ColumnProps col = this.fColumns[i];
				columns[i] = col;
			}
		}

		public void LoadFromFile(IniFile iniFile, string section)
		{
		    if (iniFile == null) return;

            foreach (Enum e in Enum.GetValues(this.fColumnEnum))
			{
				byte i = (e as IConvertible).ToByte(null);

				ColumnStatic defCol = ColumnStatics[i];

				ColumnProps col = this.fColumns[i];
				col.colType = (byte)iniFile.ReadInteger(section, "ColType_" + i.ToString(), i);
				col.colActive = iniFile.ReadBool(section, "ColActive_" + i.ToString(), defCol.active);
				col.colWidth = iniFile.ReadInteger(section, "ColWidth_" + i.ToString(), defCol.width);
				
				// protection zero/hidden columns
				if (col.colWidth <= 10) {
					col.colWidth = defCol.width;
				}
				
				this.fColumns[i] = col;
			}
		}

		public void SaveToFile(IniFile iniFile, string section)
		{
            if (iniFile == null) return;

            foreach (Enum e in Enum.GetValues(this.fColumnEnum))
			{
				byte i = (e as IConvertible).ToByte(null);

				iniFile.WriteInteger(section, "ColType_" + i.ToString(), this.fColumns[i].colType);
				iniFile.WriteBool(section, "ColActive_" + i.ToString(), this.fColumns[i].colActive);
				iniFile.WriteInteger(section, "ColWidth_" + i.ToString(), this.fColumns[i].colWidth);
			}
		}
		
		public ColumnProps GetActiveColumnByIndex(int index)
		{
			int activeIndex = -1;
			
			int num = this.fColumns.Count;
			for (int i = 0; i < num; i++)
			{
				ColumnProps colProps = this.fColumns[i];
				
				if (colProps.colActive) {
					activeIndex++;
					
					if (activeIndex == index) {
						return colProps;
					}
				}
			}
			
			return null;
		}
		
		public void WidthChanged(int colIndex, int colWidth)
		{
			ColumnProps colProps = this.GetActiveColumnByIndex(colIndex - 1); // since column "Num" excluded
			if (colProps != null) {
				colProps.colWidth = colWidth;
			}
		}
		
		public bool MoveColumn(int idx, bool up)
		{
			if (up) {
				if (idx > 0) {
					ColumnProps temp = this.fColumns[idx - 1];
					this.fColumns[idx - 1] = this.fColumns[idx];
					this.fColumns[idx] = temp;

					return true;
				}
			} else {
				if (idx >= 0 && idx < this.fColumns.Count - 1) {
					ColumnProps temp = this.fColumns[idx + 1];
					this.fColumns[idx + 1] = this.fColumns[idx];
					this.fColumns[idx] = temp;

					return true;
				}
			}

			return false;
		}
	}

}
