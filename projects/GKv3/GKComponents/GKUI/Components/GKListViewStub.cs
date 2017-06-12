/*
 * Created by SharpDevelop.
 * User: Norseman
 * Date: 08.06.2017
 * Time: 17:40
 * 
 * To change this template use Tools | Options | Coding | Edit Standard Headers.
 */
using System;
using Eto.Forms;
using GKCore.Interfaces;

namespace GKUI.Components
{
    public class GKListSubItem
    {
        
    }

    public class GKListItem : GKCore.Interfaces.IListItem
    {
        public GKListItem(object dataValue)
        {
            
        }

        public GKListItem(object dataValue, object tag)
        {
            
        }

        public void AddSubItem(object itemValue)
        {
            
        }

        public void SetBackColor(IColor color)
        {
            
        }
        
        public int CompareTo(object obj)
        {
            return 0;
        }
    }

    /// <summary>
    /// Description of GKListViewStub.
    /// </summary>
    public class GKListViewStub : Scrollable
    {
        public IListManager ListMan
        {
            get { return null; }
        }

        public GKListItem GetSelectedItem()
        {
            return null;
        }

        public object GetSelectedData()
        {
            return null;
        }

        public void UpdateContents(bool columnsChanged = false)
        {
            
        }

        public GKListItem AddItem(string text, object data)
        {
            return null;
        }

        public GKListItem AddItem(object itemValue, object data, GKListSubItem[] subitemsValues)
        {
            return null;
        }

        public void DeleteRecord(object data)
        {
            
        }

        public void BeginUpdate()
        {
            
        }

        public void EndUpdate()
        {
            
        }

        public void SelectItemByData(object data)
        {
            
        }

        public void AddColumn(string text, int width, bool autoSize = false)
        {
            
        }

        public void ClearColumns()
        {
            
        }

        public void ClearItems()
        {
            
        }

        public GKListViewStub()
        {
        }
    }
}
