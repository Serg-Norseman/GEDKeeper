using System.Collections.Generic;
using GKCore.Interfaces;

namespace GKUI.Lists
{
    public class ListFilter : IListFilter
    {
        public List<TFilterCondition> ColumnsFilter { get; private set; }
		
        public ListFilter()
        {
            this.ColumnsFilter = new List<TFilterCondition>();
        }
		
        public virtual void Clear()
        {
            this.ColumnsFilter.Clear();
        }
    }
}