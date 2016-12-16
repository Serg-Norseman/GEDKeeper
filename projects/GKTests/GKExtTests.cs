using System;
using Externals.Linguistics;
using Externals.SingleInstancing;
using NUnit.Framework;

namespace GKTests.GKCore
{
    [TestFixture]
    public class GKExtTests
    {
        [Test]
        public void RusDeclension_Tests()
        {
            Assert.AreEqual("Иванова Ивана Ивановича", RusDeclension.GetDeclension("Иванов Иван Иванович", DeclensionCase.Genitive));

            //Assert.AreEqual("атому", RusDeclension.GetDeclension("атом", DeclensionCase.Dative));
            //Assert.AreEqual("лугу", RusDeclension.GetDeclension("луг", DeclensionCase.Dative));
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
            //Assert.Throws(typeof(ArgumentNullException), () => { new SingleInstanceProxy(null); });

            string[] args = new string[1];

            using (SingleInstanceTracker tracker = new SingleInstanceTracker("test", GetSingleInstanceEnforcer)) {
                Assert.IsNotNull(tracker.Enforcer);

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
