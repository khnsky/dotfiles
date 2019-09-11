// compact
user_pref("browser.uidensity",                                              1);
// dark theme
user_pref("devtools.theme",                                                 "dark");

// disable push notifications
user_pref("dom.webnotifications.enabled",                                   false);

// restore last session
user_pref("browser.startup.page",                                           3);

// ask where to download each file
user_pref("browser.download.useDownloadDir",                                false);

// disable webrtc
user_pref("media.peerconnection.enabled",                                   false);
user_pref("media.peerconnection.turn.disable",                              true);
user_pref("media.peerconnection.use_document_iceservers",                   false);
user_pref("media.peerconnection.video.enabled",                             false);
user_pref("media.peerconnection.identity.timeout",                          1);

// tor uplift
// isolate identity sources
user_pref("privacy.firstparty.isolate",                                     true);
user_pref("privacy.resistFingerprinting",                                   true);

// mozilla tracking protection
user_pref("privacy.trackingprotection.enabled",                             true);

// don't track:
// clicks
user_pref("browser.send_pings",                                             false);
// battery status
user_pref("dom.battery.enabled",                                            false);
// copy / paste / cut events
user_pref("dom.event.clipboardevents.enabled",                              false);
// camera / mic
user_pref("media.navigator.enabled",                                        false);

// disable geolocation
user_pref("geo.enabled",                                                    false);

// disable webgl potential security risk
user_pref("webgl.disabled",                                                 true);

// disable everything on newtab page
user_pref("browser.newtabpage.activity-stream.feeds.section.highlights",    false);
user_pref("browser.newtabpage.activity-stream.feeds.snippets",              false);
user_pref("browser.newtabpage.activity-stream.migrationExpired",            false);
user_pref("browser.newtabpage.activity-stream.prerender",                   false);
user_pref("browser.newtabpage.activity-stream.showSearch",                  false);
user_pref("browser.newtabpage.activity-stream.showTopSites",                false);
user_pref("browser.newtabpage.enabled",                                     false);
user_pref("browser.newtabpage.enhanced",                                    false);
user_pref("browser.newtabpage.storageVersion"                               1);


//user_pref("browser.uiCustomization.state", "{"placements":{"widget-overflow-fixed-list":["bookmarks-menu-button","open-file-button","jid1-mnnxcxisbpnsxq_jetpack-browser-action","https-everywhere_eff_org-browser-action","jid1-bofifl9vbdl2zq_jetpack-browser-action","cookieautodelete_kennydo_com-browser-action"],"PersonalToolbar":["personal-bookmarks"],"nav-bar":["back-button","forward-button","urlbar-container","stop-reload-button","downloads-button","_testpilot-addon-browser-action","sidebar-button","jid1-mnnxcxisbpnsxq-eff_jetpack-browser-action","ublock0_raymondhill_net-browser-action"],"TabsToolbar":["tabbrowser-tabs","new-tab-button","alltabs-button"],"toolbar-menubar":["menubar-items"],"addon-bar":["addonbar-closebutton","status-bar"]},"seen":["pocket-button","developer-button","webide-button","jid1-mnnxcxisbpnsxq_jetpack-browser-action","ublock0_raymondhill_net-browser-action","https-everywhere_eff_org-browser-action","screenshots_mozilla_org-browser-action","_testpilot-addon-browser-action","jid1-bofifl9vbdl2zq_jetpack-browser-action","jid1-mnnxcxisbpnsxq-eff_jetpack-browser-action","cookieautodelete_kennydo_com-browser-action"],"dirtyAreaCache":["PersonalToolbar","nav-bar","TabsToolbar","toolbar-menubar","PanelUI-contents","addon-bar","widget-overflow-fixed-list"],"currentVersion":12,"newElementCount":4}"); 
