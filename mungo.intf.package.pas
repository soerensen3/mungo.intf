unit mungo.intf.package;

{
What is a mungo package?

  A mungo package can contain any data and is not restricted to source files. It is usually stored in a git repo.
  It can also store Lazarus packages or any other package format as well as themes used by the IDE. It can be
  installed with the mungo package manager. The package should contain a json file with the package meta information.

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  mungo.intf.filepointer;

type
  TMungoPackageManagerIntf = class abstract
    procedure InstallPackage( APackageName: String ); virtual; abstract;
    procedure UninstallPackage( APackageName: String ); virtual; abstract;
    procedure UpdatePackage( APackageName: String ); virtual; abstract;
    function SearchPackage( ASearchString: String ): TStringList; virtual; abstract;
  end;


  TMungoPackageVersionPart = ( vpMajor, vpMinor, vpBuild );
  TMungoPackageVersion = array [ TMungoPackageVersionPart ] of Integer;

  { TMungoPackageIntf }

  TMungoPackageIntf = class abstract
    private
      function GetDescription: String; virtual; abstract;
      function GetHomepage: String; virtual; abstract;
      function GetName: String; virtual; abstract;
      function GetPath: TFilePointer; virtual; abstract;
      function GetTags: TStringList; virtual; abstract;
      function GetVersion: TMungoPackageVersion; virtual; abstract;
      procedure SetDescription(AValue: String); virtual; abstract;
      procedure SetHomepage(AValue: String); virtual; abstract;
      procedure SetName(AValue: String); virtual; abstract;
      procedure SetPath(AValue: TFilePointer); virtual; abstract;
      procedure SetVersion(AValue: TMungoPackageVersion); virtual; abstract;

    public
      procedure Install; virtual; abstract;
      procedure Uninstall; virtual; abstract;

      procedure Activate; virtual; abstract;
      procedure Deactivate; virtual; abstract;

      property Version: TMungoPackageVersion read GetVersion write SetVersion;

    published
      property Name: String read GetName write SetName;
      property Path: TFilePointer read GetPath write SetPath;
      property Description: String read GetDescription write SetDescription;
      property Tags: TStringList read GetTags;
      property Homepage: String read GetHomepage write SetHomepage;
  end;



// * Get Package versions
//   git ls-remote --tags repo_url

// * Install Package
//   cd packages
//   git submodule add -f repo_url

// * Activate Package

// * Deactivate Package

// * Remove Package

// * Update Package list

// * Init Package list

// * Search Package
// - Update Package list (?)
// - Get Package list



implementation



end.

