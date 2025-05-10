import sys
import os
import config
import backup
from pathlib import Path

projectDir = Path(Path("__file__").resolve().parent)
sys.path.append(str(Path(projectDir, "profileBackup")))

class TestBackup:
    def test_full_include(self): # {{{
        parentSrcPath = Path(projectDir, "profileBackup", "tests", "test_backup_src_files")
        dstName = "test_backup_dst_full_include_files"
        config.profileConfigs.append(
            backup.Profile(
                {
                    "name": dstName,
                    "enabled": True,
                    "globPatterns": [
                        {
                            # 1. Path object
                            # 2. Path glob generator
                            # 3. string contains parent source directory(support */** wildcard character)
                            "parentSrcPath": parentSrcPath,
                            # 1. function with `parentSrcPath` as parameter
                            # 2. string
                            "versionFind": "",
                            # 1. string(either `"exclude` or `"include`)
                            "filterType": "include",
                            # 1. function with `parentSrcPath` as parameter, return boolean
                            # 2. list contains string(support */** wildcard character)
                            "filterPattern": lambda _: True,
                            # 1. boolean
                            "recursiveCopy": True,
                            "silentReport": False
                        },
                    ]
                }
            )
        )

        for s in config.profileConfigs:
            if s.name != dstName:
                s.enabled = False
            else:
                s.enabled = True

        backup.Profile.updateEnabledList()
        backup.DESTPATH       = Path(projectDir, "profileBackup", "tests", dstName)
        backup.DRYRUN         = True
        backup.SILENTMODE     = True
        backup.SHADOWTREEMODE = False
        backup.COPYSYNC       = True

        for s in config.profileConfigs:
            if s.enabled:
                s.backup()

        assert backup.Profile.fitPatBackupRelStr[dstName][str(parentSrcPath)] == [
            '0bf11eb5-8606-4520-ab6c-1610bd46b08c.dummy',
            '0c46af66-da27-4a73-8db4-e93696754bcf.dummy',
            '1709f887-18c1-4908-accd-ec948dcf65dd.dummy',
            '17fbc023-6149-401f-9375-6dcc2e0ae046.dummy',
            '18256140-4ae7-415d-8833-e022edd8f15b.dummy',
            '1f37f2c8-24c4-4079-8d9f-0085b91efa57.dummy',
            '2109e9dc-9ad8-45ce-97ee-95537fb98e3a.dummy',
            '26fc4f7c-8826-4c91-aa48-a3f036e2d991.dummy',
            '283420d8-e634-49f2-b10a-a0c4b143ecad.dummy',
            '284eac1a-7bfe-42ac-b8d2-c64402f21dd9.dummy',
            '2a4242f8-0926-412a-aae8-4ed8f1dbf599.dummy',
            '3025411f-fb77-4a66-96d0-41973b3ac3d1.dummy',
            '36815d84-a560-431a-b33f-cc60d991e868.dummy',
            '3bd88eec-72af-4874-aa12-6a33210929d4.dummy',
            '423ba5fb-8c81-4e06-bd11-9a9c51c3c2bd.dummy',
            '46fd7530-3848-48f8-a676-823345253c03.dummy',
            '4a3e32e8-5e77-4ff7-b750-70670baa2f52.dummy',
            '4de68aaf-8cc7-4f88-9eec-1e9f23a271c1.dummy',
            '57be94f1-80f9-4903-83c0-562f4250bd33.dummy',
            '5ca424fd-e87e-4635-b73d-0d20bc3e8bc3.dummy',
            '6221f8b9-1526-44fc-93f1-1f4bd2b79981.dummy',
            '6aa207ce-6e7b-40ae-9fb9-bf9948f11f68.dummy',
            '7a8584d8-685b-4f40-b45e-3e50d52f2dfa.dummy',
            '7b94369e-2a76-4640-a440-4916a31a1382.dummy',
            '7bb6e2d3-31fd-4d2b-8ec9-8c47b4d064b5.dummy',
            '7cb0b535-98a5-45e6-9d98-f6c371b239a2.dummy',
            '8613bb22-4169-4dea-8407-73baa1b7e37c.dummy',
            '885eaf64-cd39-4633-9f63-00fd69128f6a.dummy',
            '8bbe6570-23ac-4d05-af98-831b44d7d672.dummy',
            '92312bcd-4772-4905-b102-c4c52e4d4826.dummy',
            '966d1738-4d40-4071-91df-4f29daad248a.dummy',
            '98a6fee0-4867-4995-928b-c9561a1ed018.dummy',
            'a7ee216c-2c38-42b4-9e6c-cd87f6aacd57.dummy',
            'api\\apihelp.chm',
            'api\\apihelp.chw',
            'api\\API_GB.chm',
            'api\\cworksapi.chm',
            'api\\cworksapivb6.chm',
            'api\\dsgnchkapi.chm',
            'api\\dsgnchkapivb6.chm',
            'api\\emodelapi.chm',
            'api\\emodelapivb6.chm',
            'api\\emodeltoolkit.chm',
            'api\\epdmapi.chm',
            'api\\fworksapi.chm',
            'api\\fworksapivb6.chm',
            'api\\HelpViewer\\sldworksapi\\apihelpviewer.cab',
            'api\\HelpViewer\\sldworksapi\\helpcontentsetup.msha',
            'api\\HelpViewer\\swconst\\apienumshelpviewer.cab',
            'api\\HelpViewer\\swconst\\helpcontentsetup.msha',
            'api\\HTMLHelp2x\\col_sldworksapi.hxc',
            'api\\HTMLHelp2x\\col_sldworksapi.hxt',
            'api\\HTMLHelp2x\\col_sldworksapi_a.hxk',
            'api\\HTMLHelp2x\\col_sldworksapi_d.hxk',
            'api\\HTMLHelp2x\\col_sldworksapi_f.hxk',
            'api\\HTMLHelp2x\\col_sldworksapi_k.hxk',
            'api\\HTMLHelp2x\\col_sldworksapi_n.hxk',
            'api\\HTMLHelp2x\\col_swconst.hxc',
            'api\\HTMLHelp2x\\col_swconst.hxt',
            'api\\HTMLHelp2x\\col_swconst_a.hxk',
            'api\\HTMLHelp2x\\col_swconst_d.hxk',
            'api\\HTMLHelp2x\\col_swconst_f.hxk',
            'api\\HTMLHelp2x\\col_swconst_k.hxk',
            'api\\HTMLHelp2x\\col_swconst_n.hxk',
            'api\\HTMLHelp2x\\innovahxreg.exe',
            'api\\HTMLHelp2x\\sldworksapi.hxs',
            'api\\HTMLHelp2x\\swconst.hxs',
            'api\\obsoleteapi.chm',
            'api\\pdmprowebapihelp.chm',
            'api\\pdmworksapi.chm',
            'api\\pdmworksapivb6.chm',
            'api\\pidcollector.exe',
            'api\\redist\\redist.txt',
            'api\\redist\\SolidWorks.Interop.cosworks.dll',
            'api\\redist\\SolidWorks.Interop.dsgnchk.dll',
            'api\\redist\\SolidWorks.Interop.fworks.dll',
            'api\\redist\\SolidWorks.Interop.gtswutilities.dll',
            'api\\redist\\SolidWorks.Interop.sldcostingapi.dll',
            'api\\redist\\SolidWorks.Interop.sldtoolboxconfigureaddin.dll',
            'api\\redist\\SolidWorks.Interop.sldworks.dll',
            'api\\redist\\SolidWorks.Interop.sustainability.dll',
            'api\\redist\\SolidWorks.Interop.sw3dprinter.dll',
            'api\\redist\\SolidWorks.Interop.swbrowser.dll',
            'api\\redist\\SolidWorks.Interop.swcommands.dll',
            'api\\redist\\SolidWorks.Interop.swconst.dll',
            'api\\redist\\SolidWorks.Interop.swdimxpert.dll',
            'api\\redist\\SolidWorks.Interop.swdocumentmgr.dll',
            'api\\redist\\SolidWorks.Interop.swmotionstudy.dll',
            'api\\redist\\SolidWorks.Interop.swpublished.dll',
            'api\\redist\\SolidWorks.Interop.SWRoutingLib.dll',
            'api\\routingapi.chm',
            'api\\routingapivb6.chm',
            'api\\sldworksapi.chm',
            'api\\sldworksapiprogguide.chm',
            'api\\sldworksapivb6.chm',
            'api\\sustainabilityapi.chm',
            'api\\sustainabilityapivb6.chm',
            'api\\sw3dprinterapi.chm',
            'api\\sw3dprinterapivb6.chm',
            'api\\swcommands.chm',
            'api\\swconst.chm',
            'api\\swcostingapi.chm',
            'api\\swcostingapivb6.chm',
            'api\\swdimxpertapi.chm',
            'api\\swdimxpertapivb6.chm',
            'api\\swdocmgrapi.chm',
            'api\\swdocmgrapivb6.chm',
            'api\\swhtmlcontrolapi.chm',
            'api\\swhtmlcontrolapivb6.chm',
            'api\\swinspectionapi.chm',
            'api\\swinspectionapivb6.chm',
            'api\\swmotionstudyapi.chm',
            'api\\swmotionstudyapivb6.chm',
            'api\\swpublishedapi.chm',
            'api\\swpublishedapivb6.chm',
            'api\\swscanto3dapi.chm',
            'api\\swscanto3dapivb6.chm',
            'api\\swutilitiesapi.chm',
            'api\\swutilitiesapivb6.chm',
            'api\\toolboxapi.chm',
            'api\\toolboxapivb6.chm',
            'b11cf33d-03c6-46f7-82a2-5aefd2464b35.dummy',
            'b1fc58f7-55d8-40f3-bacf-a18f9ebfb7ef.dummy',
            'c2726ca2-a21b-42fc-855f-ce5e9330180c.dummy',
            'c4cf1436-4013-4b77-88e6-10f6d0ed0f82.dummy',
            'c584d22f-6f01-436b-a1ee-12b1b31aeb3e.dummy',
            'c7c68b08-e20d-48f3-b7e2-683a0e167fa9.dummy',
            'c801c97a-d9af-4e82-85e3-811ad7848247.dummy',
            'cfcf3955-5b55-4886-a7fa-4331bc566872.dummy',
            'd121c8cd-a6ea-4af8-912f-b16c22882f09.dummy',
            'd452e4d0-6ba2-4d40-a745-9c49b0ec475c.dummy',
            'd9f02199-26c6-48d6-86cc-ee7df9393f53.dummy',
            'df8916c3-4631-4e99-9efe-a1d5548c2a98.dummy',
            'e1e5a296-2a9f-4d8a-ab86-42da788dbbcf.dummy',
            'e5aa426f-548c-4abf-a202-27bcdf5750f1.dummy',
            'e686c2d5-7f38-4bdd-910e-91bbe97c5b49.dummy',
            'f1f0ddf7-e018-41b0-9296-c9377d8a9878.dummy',
            'fed45a0e-c657-4c60-bb12-584d67a1f79d.dummy'
 ] # }}}

    def test_full_sync(self): # {{{
        parentSrcPath = Path(projectDir, "profileBackup", "tests", "test_backup_src_files")
        dstName = "test_backup_dst_full_sync_files"
        versionStr = "unnamed"
        config.profileConfigs.append(
            backup.Profile(
                {
                    "name": dstName,
                    "enabled": True,
                    "globPatterns": [
                        {
                            # 1. Path object
                            # 2. Path glob generator
                            # 3. string contains parent source directory(support */** wildcard character)
                            "parentSrcPath": parentSrcPath,
                            # 1. function with `parentSrcPath` as parameter
                            # 2. string
                            "versionFind": versionStr,
                            # 1. string(either `"exclude` or `"include`)
                            "filterType": "include",
                            # 1. function with `parentSrcPath` as parameter, return boolean
                            # 2. list contains string(support */** wildcard character)
                            "filterPattern": lambda _: True,
                            # 1. boolean
                            "recursiveCopy": True,
                            "silentReport": False
                        },
                    ]
                }
            )
        )

        for s in config.profileConfigs:
            if s.name != dstName:
                s.enabled = False
            else:
                s.enabled = True

        backup.Profile.updateEnabledList()
        backup.DESTPATH       = Path(projectDir, "profileBackup", "tests", dstName)
        backup.DRYRUN         = True
        backup.SILENTMODE     = True
        backup.SHADOWTREEMODE = False
        backup.COPYSYNC       = True

        for s in config.profileConfigs:
            if s.enabled:
                s.backup()


        assert sorted(backup.Profile.syncFilesToDelete[dstName][versionStr]) == sorted([
            str(Path(backup.DESTPATH, "api", "bar", "test1.txt")),
            str(Path(backup.DESTPATH, "api", "bar", "empty_dir")) + os.path.sep,
            str(Path(backup.DESTPATH, "api", "redist", "foo")) + os.path.sep,
            str(Path(backup.DESTPATH, "foo.dummy")),
            str(Path(backup.DESTPATH, "api", "redist", "redist"))
        ])
    # }}}
