import { Component, OnInit, Renderer2, HostListener } from '@angular/core';
import { HttpClient } from '@angular/common/http';

import Map from 'ol/map';
import View from 'ol/view';
import Tile from 'ol/layer/tile';
import OSM from 'ol/source/osm';
import VectorSource from 'ol/source/vector';
import VectorLayer from 'ol/layer/vector';
import interaction from 'ol/interaction';
import Draw from 'ol/interaction/draw';
import Modify from 'ol/interaction/modify';
import Style from 'ol/style/style';
import Stroke from 'ol/style/stroke';
import controls from 'ol/control';
import Control from 'ol/control/control';
import vector from 'ol/layer/vector';

@Component({
  selector: 'app-home',
  templateUrl: './home.component.html',
  styleUrls: ['./home.component.css']
})
export class HomeComponent implements OnInit {

  territories: any = [];
  draw: Draw;

  constructor(private http: HttpClient, private renderer: Renderer2) { }

  ngOnInit() {
    this.http.get('/api/territories').subscribe((territories) => {
      this.territories = territories;
    });

    const vectorSource = new VectorSource();
    this.draw = new Draw({ source: vectorSource, type: 'Polygon' });

    const map = new Map({
      target: 'map',
      view: new View({
        center: [0, 0],
        zoom: 2
      }),
      layers: [
        new Tile({ source: new OSM() }),
        new VectorLayer({
          source: vectorSource,
          style: new Style({
            stroke: new Stroke({
              color: '#007BFF',
              width: 5
            })
          })
        })
      ],
      interactions: interaction.defaults({
        pinchRotate: false
      }).extend([
        this.draw,
        new Modify({ source: vectorSource })
      ]),
      controls: controls.defaults({
        attribution: false
      }).extend([
        this.getDeleteControl({
          onClick: () => {
            if (confirm('Do you really want to delete all the points?')) {
              vectorSource.clear();
              this.draw.setActive(false);
              this.draw.setActive(true);
            }
          }
        }),
        this.getUndoControl({
          onClick: () => {
            this.draw.removeLastPoint();
          }
        })
      ])
    });

    this.draw.on('drawend', () => {
      this.draw.setActive(false);
    });
  }

  newTerritory() {

  }

  getDeleteControl({ onClick }): Control {
    const icon = this.renderer.createElement('span');
    this.renderer.addClass(icon, 'fa');
    this.renderer.addClass(icon, 'fa-trash-o');
    this.renderer.addClass(icon, 'text-danger');
    this.renderer.setAttribute(icon, 'aria-hidden', 'true');

    const button = this.renderer.createElement('button');
    this.renderer.setAttribute(button, 'title', 'Delete All Points');
    this.renderer.listen(button, 'click', onClick);
    this.renderer.listen(button, 'touchstart', onClick);
    this.renderer.appendChild(button, icon);

    const element = this.renderer.createElement('div');
    this.renderer.addClass(element, 'delete-control');
    this.renderer.addClass(element, 'ol-unselectable');
    this.renderer.addClass(element, 'ol-control');
    this.renderer.appendChild(element, button);

    return new Control({ element });
  }

  getUndoControl({ onClick }: { onClick: () => void }): Control {
    const icon = this.renderer.createElement('span');
    this.renderer.addClass(icon, 'fa');
    this.renderer.addClass(icon, 'fa-undo');
    this.renderer.setAttribute(icon, 'aria-hidden', 'true');

    const button = this.renderer.createElement('button');
    this.renderer.setAttribute(button, 'title', 'Undo Last Point');
    this.renderer.listen(button, 'click', onClick);
    this.renderer.listen(button, 'touchstart', onClick);
    this.renderer.appendChild(button, icon);

    const element = this.renderer.createElement('div');
    this.renderer.addClass(element, 'undo-control');
    this.renderer.addClass(element, 'ol-unselectable');
    this.renderer.addClass(element, 'ol-control');
    this.renderer.appendChild(element, button);

    return new Control({ element });
  }

  @HostListener('document:keyup', ['$event'])
  onKeyUp(evt: KeyboardEvent) {
    if (evt.keyCode === 90 && evt.ctrlKey) {
      this.draw.removeLastPoint();
    }
  }

}
