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

    public class GKListItem
    {
        
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

        public object GetSelectedData()
        {
            return null;
        }

        public void UpdateContents(bool columnsChanged = false)
        {
            
        }

        public GKCore.Interfaces.IListItem AddItem(string text, object data)
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

        public void AddColumn(string text, int width, bool autoSize)
        {
            
        }

        public GKListViewStub()
        {
        }
    }
}
