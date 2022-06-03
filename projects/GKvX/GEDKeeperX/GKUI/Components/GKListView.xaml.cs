using BSLib.Design;
using BSLib.Design.MVP.Controls;
using GKCore.Interfaces;
using Xamarin.Forms;

namespace GKUI.Components
{
    public sealed partial class GKListView : View, IListViewEx
    {
        public int SelectedIndex
        {
            get { return 0; }
            set { }
        }

        public GKListView()
        {
            //InitializeComponent();
        }

        public IListViewItems Items
        {
            get {
                return null;
            }
        }

        public IListManager ListMan
        {
            get => throw new System.NotImplementedException();
            set => throw new System.NotImplementedException();
        }

        public bool Enabled { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); }

        public int SortColumn
        {
            get;
            set;
        }

        public void Activate()
        {
        }

        public void AddCheckedColumn(string caption, int width, bool autoSize = false)
        {
        }

        public IListItem AddItem(object rowData, bool isChecked, params object[] columnValues)
        {
            return null;
        }

        public void AddColumn(string caption, int width, bool autoSize)
        {
        }

        public void AddColumn(string caption, int width, bool autoSize, BSDTypes.HorizontalAlignment textAlign)
        {
        }

        public IListItem AddItem(object rowData, params object[] columnValues)
        {
            return null;
        }

        public void BeginUpdate()
        {
        }

        public void Clear()
        {
        }

        public void ClearColumns()
        {
        }

        public void ClearItems()
        {
        }

        public void DeleteRecord(object data)
        {
        }

        public void EndUpdate()
        {
        }

        public object GetSelectedData()
        {
            return null;
        }

        public void SelectItem(object rowData)
        {
        }

        public void SetColumnCaption(int index, string caption)
        {
        }

        public void SetSortColumn(int sortColumn, bool checkOrder = true)
        {
        }

        public void Sort(int sortColumn, BSDTypes.SortOrder sortOrder)
        {
        }

        public void UpdateContents(bool columnsChanged = false)
        {
        }
    }
}
