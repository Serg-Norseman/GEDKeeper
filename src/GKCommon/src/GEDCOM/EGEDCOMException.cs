using System;

namespace GKCommon.GEDCOM
{
    [Serializable]
    public class EGEDCOMException : Exception
    {
        public EGEDCOMException()
        {
        }

        public EGEDCOMException(string message) : base(message)
        {
        }
    }
}