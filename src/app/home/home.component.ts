import { Component, OnInit } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Territory } from '../territory';

// import Map from 'ol/map';
// import View from 'ol/view';
// import Tile from 'ol/layer/tile';
// import OSM from 'ol/source/osm';
// import VectorSource from 'ol/source/vector';
// import VectorLayer from 'ol/layer/vector';
// import interaction from 'ol/interaction';
// import Style from 'ol/style/style';
// import Stroke from 'ol/style/stroke';
// import controls from 'ol/control';
// import vector from 'ol/source/vector';
// import Feature from 'ol/feature';
// import Polygon from 'ol/geom/polygon';

@Component({
  selector: 'app-home',
  templateUrl: './home.component.html',
  styleUrls: ['./home.component.css']
})
export class HomeComponent implements OnInit {

  territories: Territory[] = [];

  constructor(private http: HttpClient) { }

  ngOnInit() {
    // const vectorSource = new VectorSource();

    this.http.get('/api/territories').subscribe((territories: Territory[]) => {
      this.territories = territories;
      // console.log(territories);
      // vectorSource.addFeatures(territories.map(territory => {
      //   return new Feature({
      //     geometry: new Polygon(territory.points),
      //     name: territory.name
      //   });
      // }));
    });

    // const map = new Map({
    //   target: 'map',
    //   view: new View({
    //     center: [0, 0],
    //     zoom: 2
    //   }),
    //   layers: [
    //     new Tile({ source: new OSM() }),
    //     new VectorLayer({
    //       source: vectorSource,
    //       style: new Style({
    //         stroke: new Stroke({
    //           color: '#007BFF',
    //           width: 5
    //         })
    //       })
    //     })
    //   ],
    //   interactions: interaction.defaults({
    //     pinchRotate: false
    //   }),
    //   controls: controls.defaults({
    //     attribution: false
    //   })
    // });
  }

  newTerritory() {

  }

}
