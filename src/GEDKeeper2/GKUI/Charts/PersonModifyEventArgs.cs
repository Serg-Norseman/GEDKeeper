using System;

namespace GKUI.Charts
{
	/// <summary>
	/// 
	/// </summary>
    public class PersonModifyEventArgs : EventArgs
    {
        public TreeChartPerson Person { get; set; }

        public PersonModifyEventArgs(TreeChartPerson person)
        {
            this.Person = person;
        }
    }
}