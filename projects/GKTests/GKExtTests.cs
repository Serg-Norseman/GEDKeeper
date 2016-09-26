using System;
using Externals.Linguistics;
using Externals.SingleInstancing;
using NUnit.Framework;

namespace GKTests
{
    [TestFixture]
    public class GKExtTests
    {
        public GKExtTests()
        {
        }

        [Test]
        public void RusDeclension_Tests()
        {
            Assert.AreEqual("Иванова Ивана Ивановича", RusDeclension.GetDeclension("Иванов Иван Иванович", DeclensionCase.Genitive));
        }

        #if !__MonoCS__
        private class SingleInstanceEnforcerMock: ISingleInstanceEnforcer
        {
            public void OnMessageReceived(MessageEventArgs e) { }
            public void OnNewInstanceCreated(EventArgs e) { }
        }

        [Test]
        public void SingleInstanceTracker_Tests()
        {
            string[] args = new string[1];

            using (SingleInstanceTracker tracker = new SingleInstanceTracker("test", GetSingleInstanceEnforcer)) {
                if (tracker.IsFirstInstance) {
                } else {
                    tracker.SendMessageToFirstInstance(args);
                }
            }
        }

        private static ISingleInstanceEnforcer GetSingleInstanceEnforcer()
        {
            return new SingleInstanceEnforcerMock();
        }
        #endif
    }
}
