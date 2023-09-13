using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Interfaces;
using Xamarin.Forms;
using BSDListItem = GKCore.Design.Controls.IListItem;
using BSDSortOrder = GKCore.Design.BSDTypes.SortOrder;

namespace GKUI.Components
{
    /// <summary>
    ///
    /// </summary>
    public sealed partial class GKListView : View, IListView
    {
        public bool Enabled { get => throw new System.NotImplementedException(); set => throw new System.NotImplementedException(); }

        public IListViewItems Items
        {
            get {
                return null;
            }
        }

        public IListSource ListMan
        {
            get {
                throw new System.NotImplementedException();
            }
            set {
                throw new System.NotImplementedException();
            }
        }

        public int SelectedIndex
        {
            get { return 0; }
            set { }
        }

        public bool Sorting
        {
            get;
            set;
        }

        public int SortColumn
        {
            get;
            set;
        }

        public GKListView()
        {
            //InitializeComponent();
        }

        public void Activate()
        {
        }

        public void BeginUpdate()
        {
        }

        public void EndUpdate()
        {
        }

        public void SetSortColumn(int sortColumn, bool checkOrder = true)
        {
        }

        public void Sort(int sortColumn, BSDSortOrder sortOrder)
        {
        }

        public void SortModelColumn(int columnId)
        {
        }

        public void UpdateContents(bool columnsChanged = false)
        {
        }

        public void DeleteRecord(object data)
        {
        }

        public void Clear()
        {
        }

        public void ClearColumns()
        {
        }

        public void AddColumn(string caption, int width, bool autoSize = false)
        {
        }

        public void AddCheckedColumn(string caption, int width, bool autoSize = false)
        {
        }

        public void AddColumn(string caption, int width, bool autoSize, BSDTypes.HorizontalAlignment textAlign)
        {
        }

        public void SetColumnCaption(int index, string caption)
        {
        }

        public void ResizeColumn(int columnIndex)
        {
        }

        public void ResizeColumns()
        {
        }

        public void ClearItems()
        {
        }

        public BSDListItem AddItem(object rowData, params object[] columnValues)
        {
            return null;
        }

        public BSDListItem AddItem(object rowData, bool isChecked, params object[] columnValues)
        {
            return null;
        }

        public object GetSelectedData()
        {
            return null;
        }

        public void SelectItem(int index)
        {
        }

        public void SelectItem(object rowData)
        {
        }
    }
}
