using System;
using System.Collections.Generic;
using System.Globalization;

using BSLib;
using GKCore.Interfaces;

namespace GKCore.Lists
{
	/// <summary>
	/// 
	/// </summary>
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
		
		public void Assign(ColumnProps source)
		{
			this.colType = source.colType;
			this.colActive = source.colActive;
			this.colWidth = source.colWidth;
		}
	}

	/// <summary>
	/// 
	/// </summary>
	public sealed class ColumnStatic
	{
		//public byte colType;
		public LSID colName;
		public DataType dataType;
		public NumberFormatInfo nfi;
		public string format;
		public int width;
		public bool active;
	}

    /// <summary>
    /// 
    /// </summary>
    public abstract class ListColumns : IListColumns
	{
		private List<ColumnProps> fColumns;
        private Type fColumnEnum;

		public List<ColumnStatic> ColumnStatics;
		
		public int Count
		{
			get { return this.fColumns.Count; }
		}

		public ColumnProps this[int index]
		{
			get { return this.fColumns[index]; }
			set { this.fColumns[index] = value; }
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

				this.fColumns[i] = new ColumnProps(i, cs.active, cs.width);
			}
		}

		public void Clear()
		{
			this.fColumns.Clear();
		}

		public void AddStatic(/*Enum colType*/ LSID colName, DataType dataType, int defWidth, bool defActive)
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

		public void AddStatic(/*Enum colType*/ LSID colName, DataType dataType, int defWidth, bool defActive, string format, NumberFormatInfo nfi)
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

		public void CopyTo(IListColumns columns)
		{
			ListColumns cols = columns as ListColumns;
			
            if (cols == null) {
                throw new ArgumentNullException("columns");
            }

            foreach (Enum e in Enum.GetValues(this.fColumnEnum))
			{
				byte i = (e as IConvertible).ToByte(null);

				ColumnProps col = this.fColumns[i];
				cols[i].Assign(col);
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
