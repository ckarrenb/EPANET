# Working with the Map

## Selecting a Theme to View
Node and link properties and their computed results can be viewed in color-coded fashion on the network map. To do so:
- Select the ***View*** tab on the Menu Panel.
- Use the ***Node Theme*** drop-down list to select a theme to view for network nodes.
- Use the ***Link Theme*** drop-down list to select a theme to view for network links.
- Click the ***Legend*** icons to modify the colors used to display a theme.

## Zooming In or Out
- Click the ***Zoom In*** button on the ***Map*** page of the Menu Panel to zoom in on the center of the map.
- Click the ***Zoom Out*** button on the ***Map*** page to zoom out from the center of the map.
- You can also use the mouse wheel to zoom in by moving it forward or zoom out by moving it back. The zoom will be with respect to where the mouse pointer is located.

## Scrolling the Map
To scroll the map, move the mouse with the left button pressed.

## Viewing at Full Extent
Click the ***Full Extent*** button on the ***Map*** page of the Menu Panel to view the network map at full extent.

## Re-Dimensioning the Map
- Click the ***Setup*** button on the ***Project*** page of the Menu Panel. The <a>Project Setup</a> form will appear.
- Select the ***Map*** tab on it and enter the new coordinates of the map's lower left and upper right corners. This will modify the coordinates of all network objects to fit within these boundaries yet keeping their relative positions to one another the same.

WARNING:
The map cannot be re-dimensioned when a web map service is used as a basemap.

## Locating an Object
To locate a specific object on the network map:
- Click the ***Locate*** button on the ***Project*** page of the Menu Panel.
- An ***Object Locator*** panel will appear above the Map Legend.
- Provide it with the type of object to find and its ID name.
- Press <kbd>Enter</kbd> to locate it on the map.

## Submitting a Map Query
A Map Query can be used to highlight objects on the map that meet a specific criterion. To do so:
- Click the ***Query*** button on the ***Map*** page of the Menu Panel.
- A ***Map Query*** panel will appear above the Map Legend panel.
- Specify the criterion to be used and then press <kbd>Enter</kbd>.
- All map objects that meet the criterion will be colored in red while all others will be grayed out.
- The normal object coloring will return when the ***Map Query*** panel is closed.

## Change Display Settings
You can modify how objects on the network map are drawn:
- Click the ***Settings*** button on the ***Map*** page of the Menu Panel.
- Alternatively you can simply right-click anywhere on the Map Panel.
- A ***Map Display Settings*** dialog will appear where you can select node and link sizes, choose what annotation to show, add flow direction arrows, and select a background color for the map.

## Exporting the Map
The image of the network map can be copied to the clipboard or saved to a file. Click the ***Export*** button on the ***Map*** page of the Menu Panel to make a ***Map Exporter*** panel appear above the Map Legend. Using this panel:
- Select to export the map to either the clipboard or to an image file.
- Indicate if you wish to include the Map Legend with the exported map or not.
- Click the panel's <ui2>Export</ui2> button to export the map.

If exporting to file a standard Save File dialog will appear where you can choose an image format, a location and a name for the file.

## Toggling Auto-Length
The ***Auto-Length*** feature automatically computes the length of a newly added pipe using the dimensions assigned to the network map. The current ***ON/OFF*** status of ***Auto-Length*** is displayed in EPANET's Status Panel. Its status can be changed on the ***Other*** page of the ***Map Display Settings*** dialog. 
