import { Component, OnInit } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import ol from 'ol';

@Component({
  selector: 'app-home',
  templateUrl: './home.component.html',
  styleUrls: ['./home.component.css']
})
export class HomeComponent implements OnInit {

  territories: any = [];
  map: ol.Map;

  constructor(public http: HttpClient) { }

  ngOnInit() {
    this.http.get('/api/territories').subscribe((territories) => {
      this.territories = territories;
    });
    this.map = new ol.Map({
      target: 'map',
      view: new ol.View({
        center: [0, 0],
        zoom: 1
      }),
      layers: [
        new ol.layer.Tile({
          source: new ol.source.OSM()
        })
      ]
    });
  }

  newTerritory() {

  }

}
