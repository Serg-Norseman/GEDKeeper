using System;
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Options;

namespace GKUI.Controls
{
    /// <summary>
    /// 
    /// </summary>
    [Serializable]
    public class GKListItem : ListViewItem, IComparable
    {
        protected object fValue;

        public object Data;

        public GKListItem(object itemValue, object data)
        {
        	this.fValue = itemValue;
            base.Text = this.ToString();
            this.Data = data;
        }

        public override string ToString()
        {
        	string strVal = this.fValue.ToString();
            return strVal;
        }

        public int CompareTo(object obj) 
        {
        	GKListItem otherItem = obj as GKListItem;
        	if (otherItem == null) {
        		return -1;
        	}

        	IComparable cv1 = this.fValue as IComparable;
        	IComparable cv2 = otherItem.fValue as IComparable;

        	int compRes;
			if (cv1 != null && cv2 != null)
			{
				compRes = cv1.CompareTo(cv2);
			}
			else if (cv1 != null)
			{
				compRes = -1;
			}
			else if (cv2 != null)
			{
				compRes = 1;
			}
			else {
				compRes = 0;
			}
			return compRes;
        }

        public void AddSubItem(object itemValue)
        {
        	GKListSubItem subItem = new GKListSubItem(itemValue);
        	this.SubItems.Add(subItem);
        }
    }


    public class GKListSubItem : ListViewItem.ListViewSubItem, IComparable
    {
        protected object fValue;

        public GKListSubItem(object itemValue)
        {
        	this.fValue = itemValue;
            base.Text = this.ToString();
        }

        public override string ToString()
        {
        	string strVal;

        	if (this.fValue == null) {
        		strVal = "";
        	} else {
        	    GEDCOMCustomDate customDate = this.fValue as GEDCOMCustomDate;
        	    if (customDate != null) {
        	    	GlobalOptions glob = GlobalOptions.Instance;
        			strVal = GKUtils.GetCustomDateFmtString(customDate, glob.DefDateFormat, glob.DefDateSigns, glob.ShowDatesCalendar);
        		} else {
        			strVal = this.fValue.ToString();
        		}
        	}

            return strVal;
        }

        public int CompareTo(object obj) 
        {
            GKListSubItem otherItem = obj as GKListSubItem;
        	if (otherItem == null) {
        		return -1;
        	}

        	IComparable cv1 = this.fValue as IComparable;
        	IComparable cv2 = otherItem.fValue as IComparable;

        	int compRes;
			if (cv1 != null && cv2 != null)
			{
				compRes = cv1.CompareTo(cv2);
			}
			else if (cv1 != null)
			{
				compRes = -1;
			}
			else if (cv2 != null)
			{
				compRes = 1;
			}
			else {
				compRes = 0;
			}
			return compRes;
        }
    }

}
