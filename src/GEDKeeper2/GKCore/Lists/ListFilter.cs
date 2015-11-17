using System.Collections.Generic;
using GKCore.Interfaces;

namespace GKCore.Lists
{
	/// <summary>
	/// 
	/// </summary>
    public class ListFilter : IListFilter
    {
    	private List<FilterCondition> fConditions;

        public List<FilterCondition> Conditions
        {
        	get { return this.fConditions; }
        }

        public ListFilter()
        {
            this.fConditions = new List<FilterCondition>();
        }

        public virtual void Clear()
        {
            this.fConditions.Clear();
        }
    }
}