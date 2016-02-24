using System;
using System.Collections.Generic;

using BSLib;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Types;
using GKUI.Controls;

namespace GKCore.Lists
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class ListManager : BaseObject, IListManager
	{
		protected sealed class MapColumnRec
		{
			public byte ColType;
			public byte ColSubtype;
			
			public MapColumnRec(byte colType, byte colSubtype)
			{
				this.ColType = colType;
				this.ColSubtype = colSubtype;
			}
		}

		protected ListFilter fFilter;
		protected GEDCOMTree fTree;
	    protected ExternalFilterHandler fExternalFilter;

		private readonly ListColumns fListColumns;
		private readonly List<MapColumnRec> fColumnsMap;

		public string QuickFilter = "*";

	    public ExternalFilterHandler ExternalFilter
	    {
	        get { return this.fExternalFilter; }
	        set { this.fExternalFilter = value; }
	    }

		public IListFilter Filter
		{
			get { return this.fFilter; }
		}

		public IListColumns ListColumns
		{
			get { return this.fListColumns; }
		}

	    protected ListManager(GEDCOMTree tree)
		{
			this.fTree = tree;
			this.fListColumns = GetDefaultListColumns();
			this.fColumnsMap = new List<MapColumnRec>();

			CreateFilter();
		}

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                // dummy
            }
            base.Dispose(disposing);
        }

		protected void AddListColumn(GKListView list, string caption, int width, bool autoSize, byte colType, byte colSubtype)
		{
            if (list == null) {
                throw new ArgumentNullException("list");
            }

            list.AddListColumn(caption, width, autoSize);
			this.fColumnsMap.Add(new MapColumnRec(colType, colSubtype));
		}

		protected void ColumnsMap_Clear()
		{
			fColumnsMap.Clear();
		}

		protected virtual void CreateFilter()
		{
			this.fFilter = new ListFilter();
		}

		protected static bool IsMatchesMask(string str, string mask)
		{
			bool result = false;

			if (str != null && mask != null && str != "" && mask != "")
			{
				string stx = str.ToLower();
				string[] masks = mask.ToLower().Split('|');

				int num = masks.Length;
				for (int i = 0; i < num; i++)
				{
					result = (result || GKUtils.MatchesMask(stx, masks[i]));
				}
			}

			return result;
		}

		public abstract bool CheckFilter(ShieldState shieldState);
		public abstract void Fetch(GEDCOMRecord aRec);

		protected static object GetDateValue(GEDCOMCustomEvent evt, bool isVisible)
		{
		    if (evt == null) {
				return (isVisible) ? null : (object)AbsDate.Empty();
			}

		    return GetDateValue(evt.Detail.Date.Value, isVisible);
		}

        protected static object GetDateValue(GEDCOMCustomDate date, bool isVisible)
		{
			object result;

			if (date == null) {
				result = (isVisible) ? null : (object)AbsDate.Empty();
			} else {
				if (isVisible) {
					GlobalOptions glob = GlobalOptions.Instance;
					result = GKUtils.GetCustomDateFmtString(date, glob.DefDateFormat, glob.DefDateSigns);
				} else {
					result = date.GetAbstractDate();
				}
			}

			return result;
		}

		public object GetColumnInternalValue(int colIndex)
		{
			// col_index - from 1
			MapColumnRec colrec = this.fColumnsMap[colIndex];
			return this.GetColumnValueEx(colrec.ColType, colrec.ColSubtype, false);
		}

		protected virtual object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
		{
			return null;
		}

		public virtual void InitFilter()
		{
		}

		public virtual void UpdateItem(GKListItem item, bool isMain)
		{
			if (item == null) return;

			int num = this.fColumnsMap.Count;
			for (int i = 1; i < num; i++)
			{
				MapColumnRec colrec = this.fColumnsMap[i];

				// aColIndex - from 1
				ColumnStatic cs = this.fListColumns.ColumnStatics[colrec.ColType];
				object val = GetColumnValueEx(colrec.ColType, colrec.ColSubtype, true);
				string res = ConvertColumnValue(val, cs);

				item.SubItems.Add(res);
			}
		}

		public virtual void UpdateColumns(GKListView listView, bool isMain)
		{
            if (listView == null) return;

			this.ColumnsMap_Clear();
			this.AddListColumn(listView, "№", 50, false, 0, 0);

			int num = this.fListColumns.ColumnStatics.Count;
			for (int i = 0; i < num; i++) {
				ColumnStatic cs = this.fListColumns.ColumnStatics[i];

				this.AddListColumn(listView, LangMan.LS(cs.ColName), cs.Width, false, (byte)i, 0);
			}
		}

		public string GetColumnName(Enum colType)
		{
			int col = (colType as IConvertible).ToByte(null);

            if (col >= 0 && col < fListColumns.ColumnStatics.Count) {
				return LangMan.LS(fListColumns.ColumnStatics[col].ColName);
			}

            return "<?>";
		}

		public DataType GetColumnDataType(int index)
		{
			int col = index/* - 1*/;

            if (col >= 0 && col < fListColumns.ColumnStatics.Count) {
				return fListColumns.ColumnStatics[col].DataType;
			}

            return DataType.dtString;
		}

		protected abstract ListColumns GetDefaultListColumns();

		/// <summary>
		/// Используется в блоке настройки общей фильтрации, TfmComFilter
		/// </summary>
		public abstract Type GetColumnsEnum();

		/*public Type GetColumnsEnum()
		{
			return this.fListColumns.ColumnEnum;
		}*/

		// used only in UpdateItem
		private static string ConvertColumnValue(object val, ColumnStatic cs)
		{
			if (val == null) {
				return string.Empty;
			}

			switch (cs.DataType) {
				case DataType.dtString:
					return val.ToString();

				case DataType.dtInteger:
					return val.ToString();

				case DataType.dtFloat:
					return ((double)val).ToString(cs.Format, cs.NumFmt);

				case DataType.dtDateTime:
					DateTime dtx = ((DateTime)val);
					return ((dtx.Ticks == 0) ? "" : dtx.ToString("yyyy.MM.dd HH:mm:ss", null));

				case DataType.dtGEDCOMDate:
					return val.ToString();
					
				default:
					return val.ToString();
			}
		}

		private static object ConvertColumnStr(string val, DataType type)
		{
			switch (type) {
				case DataType.dtString:
					return val;

				case DataType.dtInteger:
					return ConvHelper.ParseInt(val, 0);

				case DataType.dtFloat:
					return ConvHelper.ParseFloat(val, 0.0);

				case DataType.dtDateTime:
					return DateTime.Parse(val);

				case DataType.dtGEDCOMDate:
					return GEDCOMUtils.GetAbstractDate(val);
			}

			return val;
		}

		public void AddCondition(Enum column, ConditionKind condition, string value)
		{
			int col = (column as IConvertible).ToByte(null);
			object condValue = ConvertColumnStr(value, this.GetColumnDataType(col));

			FilterCondition fltCond = new FilterCondition(col, condition, condValue);
			this.Filter.Conditions.Add(fltCond);
		}

		private bool CheckCondition(FilterCondition fcond)
		{
            bool res = true;

            try
            {
            	object dataval = this.GetColumnValueEx(fcond.ColumnIndex, -1, false);
            	if (dataval == null) return true;

            	int compRes = 0;
            	if (fcond.Condition != ConditionKind.ck_Contains) {
                    compRes = ((IComparable)dataval).CompareTo(fcond.Value);
            	}

            	switch (fcond.Condition) {
            		case ConditionKind.ck_NotEq:
            			res = compRes != 0;
            			break;

            		case ConditionKind.ck_LT:
            			res = compRes < 0;
            			break;

            		case ConditionKind.ck_LET:
            			res = compRes <= 0;
            			break;

            		case ConditionKind.ck_Eq:
            			res = compRes == 0;
            			break;

            		case ConditionKind.ck_GET:
            			res = compRes >= 0;
            			break;

            		case ConditionKind.ck_GT:
            			res = compRes > 0;
            			break;

            		case ConditionKind.ck_Contains:
            			res = GKUtils.MatchesMask(dataval.ToString(), "*" + fcond.Value.ToString() + "*");
            			break;

            		case ConditionKind.ck_NotContains:
            			res = !GKUtils.MatchesMask(dataval.ToString(), "*" + fcond.Value.ToString() + "*");
            			break;
            	}
            }
            catch (Exception ex)
            {
            	Logger.LogWrite("ListManager.CheckCondition(): " + ex.Message);
            	res = true;
            }

            return res;
		}

		protected bool CheckCommonFilter()
		{
			bool res = true;

			try
			{
				int num = this.Filter.Conditions.Count;
				for (int i = 0; i < num; i++) {
					FilterCondition fcond = this.Filter.Conditions[i];
					res = res && this.CheckCondition(fcond);
				}
			}
			catch (Exception ex)
			{
				Logger.LogWrite("ListManager.CheckCommonFilter(): " + ex.Message);
				res = true;
			}

			return res;
		}

		private ColumnProps FindColumnProps(int colType)
		{
			int num = this.fListColumns.Count;
			for (int i = 0; i < num; i++) {
				ColumnProps props = this.fListColumns[i];
				
				if (props.ColType == colType) {
					return props;
				}
			}
			
			return null;
		}
		
		public void WidthChanged(int colIndex, int colWidth)
		{
		    if (colIndex <= 0) return;

            MapColumnRec colrec = this.fColumnsMap[colIndex];
		    ColumnProps props = this.FindColumnProps(colrec.ColType);

		    if (props != null) {
		        props.ColWidth = colWidth;
		    }
		}
	}
}
