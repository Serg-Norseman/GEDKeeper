using System.Collections.Generic;
using GKCore.Interfaces;

namespace GKUI.Lists
{
    public class ListFilter : IListFilter
    {
        public List<FilterCondition> ColumnsFilter { get; private set; }
		
        public ListFilter()
        {
            this.ColumnsFilter = new List<FilterCondition>();
        }
		
        public virtual void Clear()
        {
            this.ColumnsFilter.Clear();
        }
    }
}