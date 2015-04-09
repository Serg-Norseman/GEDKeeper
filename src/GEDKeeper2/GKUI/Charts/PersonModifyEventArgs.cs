using System;

namespace GKUI.Charts
{
    public class PersonModifyEventArgs : EventArgs
    {
        public TreeChartPerson Person { get; set; }

        public PersonModifyEventArgs(TreeChartPerson person)
        {
            this.Person = person;
        }
    }
}