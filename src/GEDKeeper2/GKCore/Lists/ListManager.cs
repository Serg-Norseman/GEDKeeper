using System;
using System.Collections.Generic;
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
		protected sealed class TColMapRec
		{
			public byte ColType;
			public byte ColSubtype;
			
			public TColMapRec(byte colType, byte colSubtype)
			{
				this.ColType = colType;
				this.ColSubtype = colSubtype;
			}
		}

		protected ListFilter fFilter;
		protected GEDCOMTree fTree;
	    protected ExternalFilterHandler fExternalFilter;

		private readonly ListColumns fListColumns;
		private readonly List<TColMapRec> fColumnsMap;

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
			this.fColumnsMap = new List<TColMapRec>();

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
			this.fColumnsMap.Add(new TColMapRec(colType, colSubtype));
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
				string[] masks = mask.ToLower().Split(new char[] { '|' });

				int num = masks.Length;
				for (int i = 0; i < num; i++)
				{
					result = (result || GKUtils.MatchesMask(stx, masks[i]));
				}
			}

			return result;
		}

		public abstract bool CheckFilter(ShieldState aShieldState);
		public abstract void Fetch(GEDCOMRecord aRec);

		protected static object GetDateValue(GEDCOMCustomEvent evt, bool isVisible)
		{
			if (evt == null) {
				object val = 0.0d;
				return (isVisible) ? null : val;
			} else {
				return GetDateValue(evt.Detail.Date.Value, isVisible);
			}
		}

		protected static object GetDateValue(GEDCOMCustomDate date, bool isVisible)
		{
			object result;

			if (date == null) {
				object val = 0.0d;
				result = (isVisible) ? null : val;
			} else {
				if (isVisible) {
					result = GKUtils.GEDCOMCustomDateToStrEx(date, GlobalOptions.Instance.DefDateFormat, false);
				} else {
					double val = GKUtils.GetAbstractDate(date);
					result = val;
				}
			}

			return result;
		}
		
		public object GetColumnInternalValue(int colIndex)
		{
			// col_index - from 1
			TColMapRec colrec = this.fColumnsMap[colIndex];
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
				TColMapRec colrec = this.fColumnsMap[i];

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

				this.AddListColumn(listView, LangMan.LS(cs.colName), cs.width, false, (byte)i, 0);
			}
		}

		public string GetColumnName(Enum colType)
		{
			int col = (colType as IConvertible).ToByte(null);

            if (col >= 0 && col < fListColumns.ColumnStatics.Count) {
				return LangMan.LS(fListColumns.ColumnStatics[col].colName);
			}

            return "<?>";
		}

		public TDataType GetColumnDataType(int index)
		{
			int col = index - 1;

            if (col >= 0 && col < fListColumns.ColumnStatics.Count) {
				return fListColumns.ColumnStatics[col].dataType;
			}

            return TDataType.dtString;
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

		private static string ConvertColumnValue(object val, ColumnStatic cs)
		{
			if (val == null) {
				return string.Empty;
			}

			switch (cs.dataType) {
				case TDataType.dtString:
					return val.ToString();

				case TDataType.dtInteger:
					return val.ToString();

				case TDataType.dtDate:
					return val.ToString();

				case TDataType.dtDateTime:
					DateTime dtx = ((DateTime)val);
					return ((dtx.Ticks == 0) ? "" : dtx.ToString("yyyy.MM.dd HH:mm:ss", null));

				case TDataType.dtFloat:
					return ((double)val).ToString(cs.format, cs.nfi);
					
				default:
					return val.ToString();
			}
		}

		private static object ConvertColumnStr(string val, TDataType type)
		{
			switch (type) {
				case TDataType.dtString:
					return val;
				case TDataType.dtInteger:
					return SysUtils.ParseInt(val, 0);
				case TDataType.dtFloat:
					return SysUtils.ParseFloat(val, 0.0);
				case TDataType.dtDate:
				case TDataType.dtDateTime:
					return DateTime.Parse(val);
			}
			return val;
		}

		public void AddCondition(Enum column, ConditionKind condition, string value)
		{
			int col = (column as IConvertible).ToByte(null);

			FilterCondition fltCond = new FilterCondition();
			fltCond.column = column;
			fltCond.col_index = col;
			fltCond.condition = condition;
			fltCond.value = ConvertColumnStr(value, this.GetColumnDataType(col));
			this.Filter.ColumnsFilter.Add(fltCond);
		}

		private bool CheckCondition(FilterCondition fcond)
		{
			object dataval = this.GetColumnValueEx(fcond.col_index, -1, false);
		    if (dataval == null) return true;

            bool res = true;

            int compRes = 0;
			if (fcond.condition != ConditionKind.ck_Contains) {
				compRes = (dataval as IComparable).CompareTo(fcond.value);
			}

			switch (fcond.condition) {
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
					res = GKUtils.MatchesMask(dataval.ToString(), "*" + fcond.value.ToString() + "*");
					break;
				case ConditionKind.ck_NotContains:
					res = !GKUtils.MatchesMask(dataval.ToString(), "*" + fcond.value.ToString() + "*");
					break;
			}
			return res;
		}

		protected bool CheckCommonFilter()
		{
			bool res = true;

			int num = this.Filter.ColumnsFilter.Count;
			for (int i = 0; i < num; i++) {
				FilterCondition fcond = this.Filter.ColumnsFilter[i];
				res = res && this.CheckCondition(fcond);
			}

			return res;
		}
		
		private ColumnProps FindColumnProps(int colType)
		{
			int num = this.fListColumns.Count;
			for (int i = 0; i < num; i++) {
				ColumnProps props = this.fListColumns[i];
				
				if (props.colType == colType) {
					return props;
				}
			}
			
			return null;
		}
		
		public void WidthChanged(int colIndex, int colWidth)
		{
			if (colIndex > 0) {
				TColMapRec colrec = this.fColumnsMap[colIndex];
				ColumnProps props = this.FindColumnProps(colrec.ColType);

				if (props != null) {
					props.colWidth = colWidth;
				}
			}
		}
	}
}
